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
    idp = 2, nmax = 100, agg.fact = 2, ...){

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

  # Read input disturbances
  if (!is.null(distEvents)){

    if (!data.table::is.data.table(distEvents)){
      distEvents <- data.table::as.data.table(distEvents)
    }
    distEvents <- unique(distEvents[
      pixelIndex %in% terra::cells(ageRast) & year %in% (yearIn - 1):yearOut,
      .(pixelIndex, year)])
    if (nrow(distEvents) == 0) distEvents <- NULL
  }

  if (!fill){

    message("Stepping ages back from ", yearIn, " to ", yearOut)
    ageRast <- ageRast - (yearIn - yearOut)

    cellsRM <- list(
      dist = distEvents$pixelIndex,
      lte0 = terra::cells(terra::classify(ageRast <= 0, cbind(FALSE, NA)))
    )

    if (length(cellsRM$dist) > 0 | length(cellsRM$lte0) > 0){

      message("Masking out ", paste(c(
        "disturbance events"[length(cellsRM$dist) > 0],
        paste("ages <=0 in", length(cellsRM$lte0), "pixels")[length(cellsRM$lte0) > 0]
      ), collapse = " and "))

      terra::set.values(ageRast, unique(do.call(c, cellsRM)), NA)
    }

    return(ageRast)

  }else{

    # Estimate ages before disturbances
    stepBack <- 1
    for (yearInit in yearIn:(yearOut + 1)){

      yearEnd <- yearInit - 1

      distCells <- if (!is.null(distEvents)) distEvents[year == yearEnd,]$pixelIndex

      if (yearEnd != yearOut & length(distCells) == 0){

        stepBack <- stepBack + 1

      }else{

        message("Estimating ages before disturbances in ", yearEnd)

        # Step ages back
        ageRast <- ageRast - stepBack

        # Reverse disturbances and fill
        if (length(distCells) > 0){
          ageRast <- gstat_replace(
            ageRast, distCells,
            ignore = -500:0,
            idp    = idp,
            nmax   = nmax,
            agg.fact = agg.fact,
            ...)
        }

        stepBack <- 1
      }

      rm(distCells)
    }

    # Replace cells with ages <=0
    cellsLTE0 <- terra::cells(terra::classify(ageRast <= 0, cbind(FALSE, NA)))
    if (length(cellsLTE0) > 0){

      message("Replacing ages <=0 in ", length(cellsLTE0), " pixels")

      ageRast <- gstat_replace(
        ageRast, cellsLTE0,
        ignore = -500:0,
        idp  = idp,
        nmax = nmax,
        agg.fact = agg.fact,
        ...)
    }

    return(ageRast)
  }
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
#' @param parallel.cores Number of cores to use in parallel processing.
#' See \code{\link[parallel]{mclapply}}.
#' @param parallel.chunkSize Number of cells to process in each parallel processing chunk.
#' @param ... additional arguments to \code{\link[gstat]{idw}} or \code{\link[gstat]{krige}}.
#'
#' @keywords internal
gstat_replace <- function(
    inRast, cells, ignore = NULL,
    idw = TRUE, idp = 2,
    nmax = Inf, maxdist = Inf, ...,
    agg.fact = 1, agg.fun = "median", agg.na.rm = TRUE,
    parallel.cores = NULL, parallel.chunkSize = 20000, verbose = TRUE){

  cells <- sort(unique(cells))
  if (length(cells) == 0){
    if (verbose) message("gstat_replace: 0 cells to replace; skipping")
    return(inRast)
  }

  if (verbose) message("gstat_replace: Reading input raster")

  smRast <- terra::deepcopy(inRast)
  terra::set.values(smRast, cells, NA)
  if (!is.null(ignore)) smRast <- terra::classify(smRast, cbind(ignore, NA))

  if (agg.fact > 1){

    smRast <- terra::aggregate(smRast, fact = agg.fact, fun = agg.fun, na.rm = agg.na.rm)

    cellsPredict <- unique(terra::cellFromXY(smRast, terra::xyFromCell(inRast, cells)))

  }else cellsPredict <- cells

  xyzIn <- terra::extract(smRast, setdiff(terra::cells(smRast), cellsPredict), xy = TRUE) |>
    data.table::data.table()
  data.table::setnames(xyzIn, names(smRast), "z")

  if (nrow(xyzIn) == 0) stop("Raster does not contain any values to use as predictors")

  if (verbose) message("gstat_replace: Predicting new values")

  gstatFunc <- if (idw) gstat::idw else gstat::krige
  xyPredict <- data.table::as.data.table(terra::xyFromCell(smRast, cellsPredict))

  if (is.null(parallel.cores)){

    newVals <- gstatFunc(
      data      = xyzIn,
      newdata   = xyPredict,
      locations = ~x+y,
      formula   = if (idw) z~1,
      idp       = if (idw) idp,
      nmax      = nmax,
      maxdist   = maxdist,
      debug.level = 0,
      ...)[,3]

  }else{

    xyPredict <- split(xyPredict, ceiling(1:nrow(xyPredict) / parallel.chunkSize))

    newVals <- parallel::mclapply(
      mc.cores = parallel.cores, mc.silent = TRUE,
      xyPredict,
      function(chunk){
        gstatFunc(
          data      = xyzIn,
          newdata   = chunk,
          locations = ~x+y,
          formula   = if (idw) z~1,
          idp       = if (idw) idp,
          nmax      = nmax,
          maxdist   = maxdist,
          debug.level = 0,
          ...)[,3]
      })
    newVals <- unname(do.call(c, newVals))
  }

  rm(xyzIn)
  rm(xyPredict)

  if (any(is.na(newVals))) stop("gstat failed to predict all replacement values. Try adjusting parameters")

  terra::set.values(smRast, cellsPredict, newVals)

  if (agg.fact > 1){

    if (verbose) message("gstat_replace: Upsampling new values")

    terra::set.values(smRast, setdiff(terra::cells(smRast), cellsPredict), NA)
    smRast <- terra::disagg(smRast, agg.fact)
    smRast <- terra::crop(smRast, inRast)

    wSize <- (agg.fact - 1) * 2 + 1
    smRast <- terra::focal(smRast, w = matrix(1, nrow = wSize, ncol = wSize), fun = mean, na.rm = TRUE)
  }

  terra::set.values(inRast, cells, terra::extract(smRast, cells)[,1])

  inRast

}


