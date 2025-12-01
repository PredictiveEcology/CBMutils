utils::globalVariables(c("x", "y", "z"))

#' Age Step Backwards
#'
#' Step an age raster backwards in time.
#' Ages that are set as <0 are replaced with surrounding ages >0 with `idw_replace`.
#'
#' @param ageRast SpatRaster. Raster with numeric values of cohort ages.
#' @param yearIn numeric. Year that ages in `ageRast` represent.
#' @param yearOut numeric. Year that ages will be stepped backwards to.
#' @param fill logical. Fill disturbed areas with values interpolated from surrounding cells.
#' @param fillLT0 logical. Fill cells with ages <0 with values interpolated from surrounding cells.
#' @param distEvents data.table. Optional.
#' Table of disturbance events with columns "pixelIndex" and "year".
#' If provided, disturbances will be reversed and the disturbed areas
#' will be filled with `idw_replace`.
#' @param agg.fact Aggregation factor for the input raster.
#' If >1, the raster will be aggregated before cell values
#' are extracted as input data points for interpolation.
#' This smooths the input data and speeds up the interpolation process.
#' See \code{\link[terra]{aggregate}}.
#' @param agg.fun Aggregation function.
#' @param agg.na.rm Remove NA cells when aggregating.
#' @param ... additional arguments to \code{\link{idw_replace}}.
#' @inheritParams idw_replace
#'
#' @return \code{SpatRaster}
#'
#' @export
ageStepBackward <- function(
    ageRast, yearIn, yearOut, distEvents = NULL,
    fill = TRUE, fillLT0 = TRUE,
    idp = 2, nmax = 100, ...,
    agg.fact = 1, agg.fun = "median", agg.na.rm = TRUE){

  if (yearIn == yearOut) return(ageRast)
  if (yearIn <  yearOut) stop("Year input is less than year output. Use `ageStepForward`")

  if (length(find.package("withr", quiet = TRUE)) == 0) stop(
    "withr package required. Install with `install.packages(\"withr\")`")

  # Set temporary directories for intermediate data; unlink and reset on close
  tmpdir <- file.path(getOption("spades.scratchPath", default = tempdir()), "CBMutils")

  terraDirInit <- evalq(terra::terraOptions(print = FALSE)[c("tempdir", "memfrac")])
  withr::defer(do.call(terra::terraOptions, terraDirInit))
  terra::terraOptions(
    tempdir = withr::local_tempdir("terra_", tmpdir = tmpdir),
    memfrac = 0)
  withr::local_options(list(rasterTmpDir = withr::local_tempdir("raster_", tmpdir = tmpdir)))

  if (!is.null(distEvents) && !data.table::is.data.table(distEvents)){
    distEvents <- data.table::as.data.table(distEvents)
  }

  if (!fill){

    message("Stepping ages back from ", yearIn, " to ", yearOut)
    ageRast <- ageRast - (yearIn - yearOut)

    if (!is.null(distEvents)) distEvents <- distEvents[
      pixelIndex %in% terra::cells(ageRast) & year %in% (yearIn - 1):yearOut,
      .(pixelIndex, year)]

    cellsRM <- list(
      dist = distEvents$pixelIndex,
      lte0 = terra::cells(terra::classify(ageRast < 0, cbind(FALSE, NA)))
    )

    if (length(cellsRM$dist) > 0 | length(cellsRM$lte0) > 0){

      message("Masking out ", paste(c(
        "disturbance events"[length(cellsRM$dist) > 0],
        paste("ages <0 in", length(cellsRM$lte0), "pixels")[length(cellsRM$lte0) > 0]
      ), collapse = " and "))

      terra::set.values(ageRast, unique(do.call(c, cellsRM)), NA)
    }

    return(ageRast)
  }

  if (agg.fact > 1) stop("agg.fact cannot yet be >= 1")

  # Read input
  cellsIn <- terra::cells(ageRast)

  cxyz <- data.table::as.data.table(terra::extract(ageRast, cellsIn, xy = TRUE))
  data.table::setnames(cxyz, names(cxyz)[[3]], "z")
  cxyz[, c := cellsIn]
  cxyz[, x := x - min(x)]
  cxyz[, y := y - min(y)]
  data.table::setkey(cxyz, c)
  data.table::setcolorder(cxyz)

  stepBack <- 1
  for (yearInit in yearIn:(yearOut + 1)){

    yearEnd <- yearInit - 1

    distCells <- if (!is.null(distEvents)) intersect(distEvents[year == yearEnd,]$pixelIndex, cellsIn)

    if (yearEnd != yearOut & length(distCells) == 0){

      stepBack <- stepBack + 1

    }else{

      message("Stepping ages back from ", yearEnd + stepBack, " to ", yearEnd)

      cxyz[, z := z - stepBack]

      message("Estimating ages before disturbances in ", yearEnd)

      if (length(distCells) > 0){
        cxyz <- idw_replace(
          cxyz, distCells,
          ignore = -500:-1,
          idp    = idp,
          nmax   = nmax,
          ...)
      }

      stepBack <- 1
    }

    rm(distCells)
  }
  rm(cellsIn)

  # Replace cells with ages <0
  if (fillLT0){
    cellsLTE0 <- cxyz[z < 0,]$c
    if (length(cellsLTE0) > 0){

      message("Replacing ages <0 in ", length(cellsLTE0), " pixels")

      cxyz <- idw_replace(
        cxyz, cellsLTE0,
        ignore = -500:-1,
        idp  = idp,
        nmax = nmax,
        ...)
    }
  }

  ageRast <- terra::rast(ageRast)
  terra::set.values(ageRast, cxyz$c, cxyz$z)

  return(ageRast)

}

#' IDW replace
#'
#' Replace values in a table by interpolating their values
#' from the other points values using IDW interpolation.
#'
#' @param cxyz Table with xy coordinates, z values, and point IDs (c).
#' @param ignore numeric. Raster values to exclude from input data.
#' @param idp numeric. IDW power.
#' @param nmax numeric. The maximum number of nearest observations to use.
#' @param parallel.cores Number of cores to use in parallel processing.
#' See \code{\link[parallel]{mclapply}}.
#' @param parallel.chunkSize Number of cells to process in each parallel processing chunk.
#' @param ... additional arguments to \code{\link[parallel]{mclapply}} or \code{\link[parallel]{mclapply}}.
#'
#' @keywords internal
idw_replace <- function(
    cxyz, cells, ignore = NULL,
    idp = 2, nmax = Inf,
    parallel.cores = NULL, parallel.chunkSize = 25000, ...,
    verbose = TRUE){

  if (length(find.package("FNN", quiet = TRUE)) == 0) stop(
    "FNN package required. Install with `install.packages(\"FNN\")`")

  cells <- sort(unique(cells))
  if (length(cells) == 0){
    if (verbose) message("idw_replace: 0 cells to replace; skipping")
    return(c())
  }

  if (verbose) message("idw_replace: Reading inputs")

  rep <- inp <- ch <- NULL

  cxyz[, rep := c %in% cells]
  cxyz[, inp := !rep & !z %in% ignore]

  if (sum(cxyz$inp) == 0) stop("No input values to predict from")

  if (verbose) message("idw_replace: Predicting new values")

  idw <- function(xyIn, zIn, xyOut, k, p, ...){

    nn <- FNN::get.knnx(xyIn, query = xyOut, k = min(k, nrow(xyIn)), ...)

    sapply(1:nrow(xyOut), function(i){
      w <- 1 / (nn$nn.dist[i,] ^ p)
      sum(w * zIn[nn$nn.index[i,]]) / sum(w)
    })
  }

  if (is.null(parallel.cores) || is.na(parallel.cores)){

    newVals <- idw(
      cxyz[inp == TRUE, .(x, y)],
      cxyz[inp == TRUE]$z,
      cxyz[rep == TRUE, .(x, y)],
      p = idp, k = nmax)

  }else{

    cxyz[rep == TRUE, ch := ceiling(1:length(cells) / parallel.chunkSize)]

    newVals <- parallel::mclapply(
      mc.cores = parallel.cores, mc.silent = TRUE, ...,
      na.omit(unique(cxyz$ch)),
      function(chunk) idw(
        cxyz[inp == TRUE, .(x, y)],
        cxyz[inp == TRUE]$z,
        cxyz[ch == chunk, .(x, y)],
        p = idp, k = nmax)
    )
    newVals <- unname(do.call(c, newVals))
    cxyz[, ch := NULL]
  }

  if (verbose) message("idw_replace: Setting new values")

  cxyz[rep == TRUE, z := newVals]
  cxyz[, inp := NULL]
  cxyz[, rep := NULL]

  cxyz

}


