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
    idp = 1, nmax = 100, agg.fact = 2, agg.fun = "median", ...){

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

    if (!is.null(distEvents)){

      message("Masking out disturbance events")
      terra::set.values(ageRast, unique(distEvents$pixelIndex), NA)
      distEvents <- NULL
    }

    cellsLTE0 <- terra::cells(terra::classify(ageRast <= 0, cbind(FALSE, NA)))
    if (length(cellsLTE0) > 0){

      message("Removing ages <=0 in ", length(cellsLTE0), " pixels")
      terra::set.values(ageRast, cellsLTE0, NA)
    }

    return(ageRast)

  }else{

    # Aggregate input raster
    if (!is.null(distEvents) & agg.fact > 1){

      message("Aggregating input age raster")

      stepRast <- terra::classify(ageRast, matrix(c(-Inf, 0, NA), ncol = 3, byrow = TRUE))
      stepRast <- terra::aggregate(stepRast, fact = agg.fact, fun = agg.fun, na.rm = TRUE)

      if (!is.null(distEvents)){
        distEvents <- cbind(distEvents, terra::xyFromCell(ageRast, distEvents$pixelIndex))
        data.table::setnames(distEvents, "pixelIndex", "pixelIndexIn")
        distEvents[, pixelIndex   := terra::cellFromXY(stepRast, distEvents[, .(x, y)])]
        distEvents[, c("x", "y") := NULL]
      }

    }else stepRast <- ageRast

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
        stepRast <- stepRast - stepBack

        # Reverse disturbances and fill
        if (length(distCells) > 0){
          stepRast <- gstat_replace(
            stepRast, distCells,
            ignore = -500:0,
            idp    = idp,
            nmax   = nmax,
            ...)
        }

        stepBack <- 1
      }

      rm(distCells)
    }

    # Disaggregate result
    if (!is.null(distEvents) & agg.fact > 1){

      message("Resampling output")

      stepRast <- terra::disagg(stepRast, agg.fact)
      stepRast <- terra::crop(stepRast, ageRast)

      mSize <- (agg.fact - 1) * 2 + 1
      stepRast <- terra::focal(stepRast, w = matrix(1, nrow = mSize, ncol = mSize),
                               fun = mean, na.rm = TRUE)

      terra::set.values(stepRast, setdiff(1:terra::ncell(stepRast), distEvents$pixelIndexIn), NA)
      stepRast <- terra::cover(stepRast, ageRast - (yearIn - yearOut))
    }

    # Replace cells with ages <=0
    cellsLTE0 <- terra::cells(terra::classify(stepRast <= 0, cbind(FALSE, NA)))
    if (length(cellsLTE0) > 0){

      message("Replacing ages <=0 in ", length(cellsLTE0), " pixels")

      stepRast <- gstat_replace(
        stepRast, cellsLTE0,
        idp  = idp,
        nmax = nmax,
        agg.fact = agg.fact,
        agg.fun  = agg.fun,
        ...)
    }

    return(stepRast)
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
    parallel.cores = 1, parallel.chunkSize = 20000, verbose = TRUE){

  cells <- sort(unique(cells))
  if (length(cells) == 0){
    if (verbose) message("gstat_replace: 0 cells to replace; skipping")
    return(inRast)
  }

  # Set input arguments
  inArgs  <- list(
    locations = ~x+y,
    formula   = if (idw) z~1,
    idp       = if (idw) idp,
    nmax      = nmax,
    maxdist   = maxdist,
    debug.level = 0
  )
  dotArgs <- list(...)
  inArgs <- c(inArgs[!sapply(inArgs, is.null) & !names(inArgs) %in% names(dotArgs)], dotArgs)
  rm(dotArgs)

  if (verbose) message("gstat_replace: Reading input raster")
  if (agg.fact > 1){

    smRast <- terra::deepcopy(inRast)
    terra::set.values(smRast, cells, NA)
    if (!is.null(ignore)) smRast <- terra::classify(smRast, cbind(ignore, NA))
    smRast <- terra::aggregate(smRast, fact = agg.fact, fun = agg.fun, na.rm = agg.na.rm)

    inArgs$data <- terra::extract(smRast, terra::cells(smRast), xy = TRUE) |>
      data.table::data.table()
    data.table::setnames(inArgs$data, names(inRast), "z")

    cellsIn <- cells
    cells <- unique(terra::cellFromXY(smRast, terra::xyFromCell(inRast, cells)))

  }else{

    inArgs$data <- terra::extract(inRast, setdiff(terra::cells(inRast), cells), xy = TRUE) |>
      data.table::data.table()
    data.table::setnames(inArgs$data, names(inRast), "z")

    if (!is.null(ignore)) inArgs$data <- inArgs$data[!z %in% ignore,]
  }

  if (nrow(inArgs$data) == 0) stop("Raster does not contain any values to use as predictors")

  if (verbose) message("gstat_replace: Predicting new values")

  func <- if (idw) gstat::idw else gstat::krige

  if (parallel.cores == 1){

    inArgs$newdata <- data.table::data.table(terra::xyFromCell(inRast, cells))

    newVals <- do.call(func, inArgs)[,3]

  }else{

    newVals <- parallel::mclapply(
      mc.cores = parallel.cores, mc.silent = TRUE,
      split(cells, ceiling(1:length(cells) / parallel.chunkSize)),
      function(chunk){
        newdata <- data.table::data.table(terra::xyFromCell(inRast, chunk))
        do.call(func, c(list(newdata = newdata), inArgs))[,3]
      })
    newVals <- unname(do.call(c, newVals))
  }

  rm(inArgs)

  if (any(is.na(newVals))) stop("gstat failed to predict all replacement values. Try adjusting parameters")

  if (agg.fact > 1){

    if (verbose) message("gstat_replace: Resampling new values")

    smRast <- terra::rast(smRast, vals = NA)
    terra::set.values(smRast, cells, newVals)
    smRast <- terra::disagg(smRast, agg.fact)
    smRast <- terra::crop(smRast, inRast)

    mSize <- (agg.fact - 1) * 2 + 1
    smRast <- terra::focal(smRast, w = matrix(1, nrow = mSize, ncol = mSize), fun = mean, na.rm = TRUE)

    cells <- cellsIn
    newVals <- terra::extract(smRast, cellsIn)[,1]

    rm(cellsIn)
    rm(smRast)
  }

  terra::set.values(inRast, cells, newVals)

  inRast

}


