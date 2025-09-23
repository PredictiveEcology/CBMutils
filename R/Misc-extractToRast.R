
#' Extract to raster
#'
#' Extract values from a spatial data source for each cell of a template raster.
#' Resampling is done using "mode" resampling to preserve categorical values.
#'
#' @param input terra SpatRaster, one or more raster files, or sf polygons.
#' @param templateRast SpatRaster. Raster template.
#' @param index numeric or character. Raster layer or vector field to extract.
#'
#' @export
#' @return vector with a value for each cell of `templateRast`.
#' Data type matches input data type.
extractToRast <- function(input, templateRast, index = 1){

  if (length(find.package("exactextractr", quiet = TRUE)) == 0) stop(
    "exactextractr package required. Install with `install.packages(\"exactextractr\")`")
  if (length(find.package("withr", quiet = TRUE)) == 0) stop(
    "withr package required. Install with `install.packages(\"withr\")`")

  # Set temporary directories for intermediate data; unlink and reset on close
  tmpdir <- file.path(getOption("spades.scratchPath", default = tempdir()), "CBMutils")

  terraDirInit <- evalq(terra::terraOptions(print = FALSE)[["tempdir"]])
  withr::defer(terra::terraOptions(tempdir = terraDirInit, print = FALSE))
  terra::terraOptions(tempdir = withr::local_tempdir("terra_", tmpdir = tmpdir), print = FALSE)

  withr::local_options(list(rasterTmpDir = withr::local_tempdir("raster_", tmpdir = tmpdir)))

  if (basename(dirname(terra::terraOptions(print = FALSE)[["tempdir"]])) != "CBMutils") stop()
  if (is.null(getOption("rasterTmpDir")) || basename(dirname(getOption("rasterTmpDir"))) != "CBMutils") stop()

  if (inherits(input, "sf")){
    extractToRast_vect(input, templateRast, field = index)

  }else{
    extractToRast_rast(input, templateRast, layer = index)
  }
}

# Extract values from spatial data source: raster
extractToRast_rast <- function(input, templateRast, layer = 1){

  # Read as SpatRaster; mosaic tiles if need be
  if (!inherits(input, "SpatRaster")){

    if (length(input) > 1 && is.character(input) &&
        all(tryCatch(file.exists(input), error = function(e) FALSE))){
      input <- do.call(terra::mosaic, lapply(input, terra::rast))

    }else{
      input <- terra::rast(input)
    }
  }

  # Select raster layer
  input <- terra::subset(input, layer)

  # Get raster categories
  cats <- terra::cats(input)[[1]]

  # Crop and reproject
  reproject <- !terra::compareGeom(
    input, templateRast,
    crs = TRUE, warncrs = FALSE, stopOnError = FALSE, messages = FALSE,
    lyrs = FALSE, ext = FALSE, rowcol = FALSE, res = FALSE)

  input <- terra::crop(
    input,
    terra::project(terra::as.polygons(templateRast, extent = TRUE), terra::crs(input)),
    snap = "out")

  ## Reclassify if contains NAs
  valUq <- terra::unique(input, na.rm = FALSE)
  if (length(valUq[,1]) == 1){
    return(rep(valUq[1,1], terra::ncell(templateRast)))
  }
  if (any(c(NA, NaN) %in% valUq[,1])){
    valUq <- data.table(inp = rev(valUq[,1]), temp = 1:nrow(valUq))
    if (!is.null(cats)) valUq$inp <- match(valUq$inp, cats[[2]])
    input <- terra::classify(input, valUq)
  }else valUq <- NULL

  if (reproject){
    input <- terra::project(input, templateRast, method = "mode")

  }else{
    terra::crs(input) <- terra::crs(templateRast)
    input <- exactextractr::exact_resample(input, templateRast, fun = "mode")
  }

  # Extract and return raster values
  alignVals <- terra::values(input, mat = FALSE)
  if (!is.null(valUq)) alignVals <- valUq$inp[alignVals]
  if (!is.null(cats))  alignVals <- cats[match(alignVals, cats[[1]]), -1]
  alignVals

}

# Extract values from spatial data source: vector
extractToRast_vect <- function(input, templateRast, field = 1){

  # Crop and reproject
  reproject <- !terra::compareGeom(
    terra::rast(crs = terra::crs(input)), templateRast,
    crs = TRUE, warncrs = FALSE, stopOnError = FALSE, messages = FALSE,
    lyrs = FALSE, ext = FALSE, rowcol = FALSE, res = FALSE)

  cropBBOX <- sf::st_as_sfc(sf::st_bbox(templateRast)) |>
    sf::st_buffer(terra::res(templateRast)[[1]], joinStyle = "MITRE", mitreLimit = 2)

  .muffleWarningAgr <- function(w){
    agrWarning <- "attribute variables are assumed to be spatially constant throughout all geometries"
    if (w$message == agrWarning) invokeRestart("muffleWarning")
  }

  if (reproject){

    cropBBOX <- cropBBOX |>
      sf::st_segmentize(10000) |>
      sf::st_transform(sf::st_crs(input))

    input <- withCallingHandlers(
      sf::st_intersection(input, sf::st_transform(cropBBOX, sf::st_crs(input))),
      warning = .muffleWarningAgr)
    sf::st_geometry(input) <- sf::st_transform(sf::st_geometry(input), sf::st_crs(templateRast))

  }else{

    suppressWarnings(sf::st_crs(cropBBOX) <- sf::st_crs(input))

    input <- withCallingHandlers(
      sf::st_crop(input, cropBBOX),
      warning = .muffleWarningAgr)
  }

  # Dissolve polygons
  if (any(duplicated(input[[field]]))){
    input <- dplyr::summarise(input, geometry = sf::st_union(geometry), .by = field)
  }

  # Rasterize
  cellIdxRast <- exactextractr::rasterize_polygons(
    input, templateRast, min_coverage = 0.5)

  # Return values
  input[[field]][terra::values(cellIdxRast)]
}


#' Write raster with values
#'
#' Replace the values in a `SpatRaster` before writing it to file.
#'
#' @param templateRast terra `SpatRaster` template defining raster geometry.
#' @param values numeric or character. New raster values.
#' @param filename character. Output filename.
#' @param ... arguments to \code{\link[terra]{writeRaster}}
#'
#' @export
writeRasterWithValues <- function(templateRast, values, filename, ...){

  if (!is.numeric(values)) values <- factor(values)
  terra::values(templateRast) <- values

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(templateRast, filename = filename, ...)

  return(invisible())
}

