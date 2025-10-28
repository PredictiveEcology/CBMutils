utils::globalVariables(c("x", "y", "z"))

#' Age Step Backwards
#'
#' Step an age raster backwards in time.
#' Ages that are set as <=0 are replaced with surrounding ages >0 with `gstat_replace`.
#'
#' @param ageRast SpatRaster. Raster with numeric values of cohort ages.
#' @param yearIn numeric. Year that ages in `ageRast` represent.
#' @param yearOut numeric. Year that ages will be stepped backwards to.
#' @param fill logical. Fill disturbed areas and cells with ages <= 0 with values from surrounding cells.
#' @param distEvents data.table. Optional.
#' Table of disturbance events with columns "pixelIndex" and "year".
#' If provided, disturbances will be reversed and the disturbed areas
#' will be filled with `gstat_replace`.
#' @inheritParams gstat_replace
#'
#' @return \code{SpatRaster}
#'
#' @export
ageStepBackward <- function(
    ageRast, yearIn, yearOut, fill = TRUE, distEvents = NULL,
    idw = TRUE, formula = z~1, agg.fact = 2, agg.fun = "median", ...){

  if (yearIn == yearOut) return(ageRast)
  if (yearIn <  yearOut) stop("Year input is less than year output. Use `ageStepForward`")

  if (length(find.package("gstat", quiet = TRUE)) == 0) stop(
    "gstat package required. Install with `install.packages(\"gstat\")`")
  if (length(find.package("withr", quiet = TRUE)) == 0) stop(
    "withr package required. Install with `install.packages(\"withr\")`")

  # Set temporary directories for intermediate data; unlink and reset on close
  tmpdir <- file.path(getOption("spades.scratchPath", default = tempdir()), "CBMutils")

  terraDirInit <- evalq(terra::terraOptions(print = FALSE)[["tempdir"]])
  withr::defer(terra::terraOptions(tempdir = terraDirInit, print = FALSE))
  terra::terraOptions(tempdir = withr::local_tempdir("terra_", tmpdir = tmpdir), print = FALSE)

  withr::local_options(list(rasterTmpDir = withr::local_tempdir("raster_", tmpdir = tmpdir)))

  if (is.null(distEvents) | !fill){

    if (!is.null(distEvents)){

      message("Masking out disturbance events")

      terra::set.values(
        ageRast,
        unique(subset(distEvents, year %in% (yearIn - 1):yearOut)$pixelIndex),
        NA)
    }

    message("Stepping ages back from ", yearIn, " to ", yearOut)

    ageRast <- ageRast - (yearIn - yearOut)

  }else{

    cellsInit <- terra::cells(ageRast)

    for (yearInit in yearIn:(yearOut + 1)){

      yearEnd <- yearInit - 1

      message("Stepping ages back from ", yearInit, " to ", yearEnd)

      # Step ages back 1 year
      ageRast <- ageRast - 1

      # Reverse disturbances and fill
      if (!is.null(distEvents)){
        ageRast <- gstat_replace(
          inRast   = ageRast,
          cells    = intersect(cellsInit, subset(distEvents, year == yearEnd)$pixelIndex),
          ignore   = -500:0,
          idw      = idw,
          formula  = formula,
          agg.fact = agg.fact,
          agg.fun  = agg.fun,
          ...)
      }
    }
  }

  # Replace cells with ages <=0
  if (terra::global(ageRast, "min", na.rm = TRUE) <= 0){

    cellsLTE0 <- terra::cells(terra::classify(ageRast <= 0, cbind(FALSE, NA)))

    if (fill){

      message("Replacing ages <=0 in ", length(cellsLTE0), " pixels")

      ageRast <- gstat_replace(
        inRast   = ageRast,
        cells    = cellsLTE0,
        idw      = idw,
        formula  = formula,
        agg.fact = agg.fact,
        agg.fun  = agg.fun,
        ...)

    }else{

      message("Removing ages <=0 in ", length(cellsLTE0), " pixels")

      terra::set.values(ageRast, cellsLTE0, NA)
    }
  }

  return(ageRast)
}


#' gstat replace cells
#'
#' Replace cells in a raster by interpolating their values
#' from the other cells values with gstat::idw or gstat::krige.
#'
#' @param ignore numeric. Raster values to exclude from input data.
#' @param idw logical. Use \code{\link[gstat]{idw}} or \code{\link[gstat]{krige}}.
#' @param formula Interpolation formula.
#' @param agg.fact Aggregation factor for the input raster.
#' If >1, the raster will be aggregated before cell values
#' are extracted as input data points for interpolation.
#' This smooths the input data and speeds up the interpolation process.
#' See \code{\link[terra]{aggregate}}.
#' @param agg.fun Aggregation function.
#' @param agg.na.rm Remove NA cells in aggregation function.
#' @param ... additional arguments to \code{\link[gstat]{idw}} or \code{\link[gstat]{krige}}.
#'
#' @keywords internal
gstat_replace <- function(
    inRast, cells, ignore = NULL,
    idw = TRUE, formula = z~1, ...,
    agg.fact = 2, agg.fun = "median", agg.na.rm = TRUE,
    verbose = TRUE){

  if (length(cells) == 0){
    if (verbose) message("gstat_replace: 0 cells to replace; skipping")
    return(inRast)
  }

  if (verbose) message("gstat_replace: Removing values in fill cells")

  names(inRast) <- "z"
  terra::set.values(inRast, cells, NA)

  if (agg.fact > 1){

    if (verbose) message("gstat_replace: Aggregating input raster")

    if (!is.null(ignore)){
      smRast <- terra::classify(inRast, cbind(ignore, NA))
    }else smRast <- inRast
    smRast <- terra::aggregate(smRast, fact = agg.fact, fun = agg.fun, na.rm = agg.na.rm)

  }else smRast <- inRast

  if (verbose) message("gstat_replace: Predicting values for fill cells")

  inData <- terra::extract(smRast, terra::cells(smRast), xy = TRUE)
  if (!is.null(ignore) & !agg.fact > 1){
    inData <- subset(inData, !z %in% ignore)
  }

  if (nrow(inData) == 0) stop("Raster does not contain any values to use as predictors")

  gstatFunc <- if (idw) gstat::idw else gstat::krige
  newVals <- gstatFunc(
    formula     = formula,
    locations   = ~x+y,
    data        = inData,
    newdata     = as.data.frame(terra::xyFromCell(inRast, cells)),
    debug.level = 0,
    ...
  )[, 3]
  rm(inData)

  if (any(is.na(newVals))) stop(
    "gstat failed to predict all replacement values. Try adjusting parameters")

  terra::set.values(inRast, cells, newVals)

  inRast

}


