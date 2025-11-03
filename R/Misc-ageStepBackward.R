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
#' @param ... arguments to \code{\link{gstat_replace}}.
#' @inheritParams gstat_replace
#'
#' @return \code{SpatRaster}
#'
#' @export
ageStepBackward <- function(
    ageRast, yearIn, yearOut, fill = TRUE, distEvents = NULL,
    idp = 1, nmax = 500, ...){

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

  # Copy input raster
  ageRast <- terra::deepcopy(ageRast)

  if (!is.null(distEvents)){

    if (!data.table::is.data.table(distEvents)){
      distEvents <- data.table::as.data.table(distEvents)
    }
    distEvents <- unique(distEvents[
      pixelIndex %in% terra::cells(ageRast) & year %in% (yearIn - 1):yearOut,
      .(pixelIndex, year)])
    if (nrow(distEvents) == 0) distEvents <- NULL
  }

  if (!is.null(distEvents) & !fill){

    message("Masking out disturbance events")

    terra::set.values(ageRast, unique(distEvents$pixelIndex), NA)
    distEvents <- NULL
  }

  stepBack <- 1
  for (yearInit in yearIn:(yearOut + 1)){

    yearEnd <- yearInit - 1

    distCells <- if (!is.null(distEvents)) distEvents[year == yearEnd,]$pixelIndex

    if (yearEnd != yearOut & length(distCells) == 0){

      stepBack <- stepBack + 1

    }else{

      message("Stepping ages back from ", yearEnd + stepBack, " to ", yearEnd)

      # Step ages back
      ageRast <- ageRast - stepBack

      # Reverse disturbances and fill
      if (length(distCells) > 0){
        ageRast <- gstat_replace(
          ageRast, distCells,
          ignore = -500:0,
          idp    = idp,
          nmax   = nmax,
          ...)
      }

      stepBack <- 1
    }

    rm(distCells)
  }

  # Replace cells with ages <=0
  if (terra::global(ageRast, "min", na.rm = TRUE) <= 0){

    cellsLTE0 <- terra::cells(terra::classify(ageRast <= 0, cbind(FALSE, NA)))

    if (fill){

      message("Replacing ages <=0 in ", length(cellsLTE0), " pixels")

      ageRast <- gstat_replace(
        ageRast, cellsLTE0,
        idp    = idp,
        nmax   = nmax,
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
#' @param idp numeric. IDW power.
#' @param nmax numeric. The maximum number of nearest observations to use.
#' @param maxdist numeric. The maximum distance of observations to use.
#' @param agg.fact Aggregation factor for the input raster.
#' If >1, the raster will be aggregated before cell values
#' are extracted as input data points for interpolation.
#' This smooths the input data and speeds up the interpolation process.
#' See \code{\link[terra]{aggregate}}.
#' @param agg.fun Aggregation function.
#' @param agg.na.rm Remove NA cells when aggregating.
#' @param ... additional arguments to \code{\link[gstat]{idw}} or \code{\link[gstat]{krige}}.
#'
#' @keywords internal
gstat_replace <- function(
    inRast, cells, ignore = NULL,
    idw = TRUE, idp = 2,
    nmax = Inf, maxdist = Inf, ...,
    agg.fact = 1, agg.fun = "median", agg.na.rm = TRUE,
    verbose = TRUE){

  if (length(cells) == 0){
    if (verbose) message("gstat_replace: 0 cells to replace; skipping")
    return(inRast)
  }

  if (verbose) message("gstat_replace: Removing old values")
  terra::set.values(inRast, cells, NA)

  smRast <- inRast
  if (agg.fact > 1){

    if (verbose) message("gstat_replace: Aggregating input raster")

    if (!is.null(ignore)) smRast <- terra::classify(smRast, cbind(ignore, NA))
    smRast <- terra::aggregate(smRast, fact = agg.fact, fun = agg.fun, na.rm = agg.na.rm)
  }

  if (verbose) message("gstat_replace: Predicting new values")

  inArgs  <- list(
    debug.level = 0,
    locations = ~x+y,
    data      = data.table::data.table(terra::extract(smRast, terra::cells(smRast), xy = TRUE)),
    newdata   = data.table::data.table(terra::xyFromCell(inRast, cells)),
    formula   = if (idw) z~1,
    idp       = if (idw) idp,
    nmax      = nmax,
    maxdist   = maxdist
  )
  data.table::setnames(inArgs$data, names(inArgs$data)[[3]], "z")
  if (!is.null(ignore) & agg.fact == 1) inArgs$data <- inArgs$data[!z %in% ignore,]
  if (nrow(inArgs$data) == 0) stop("Raster does not contain any values to use as predictors")

  dotArgs <- list(...)
  inArgs <- c(inArgs[!sapply(inArgs, is.null) & !names(inArgs) %in% names(dotArgs)], dotArgs)
  rm(dotArgs)

  newVals <- do.call(if (idw) gstat::idw else gstat::krige, inArgs)[,3]
  rm(inArgs)

  if (any(is.na(newVals))) stop("gstat failed to predict all replacement values. Try adjusting parameters")

  terra::set.values(inRast, cells, newVals)

  inRast

}


