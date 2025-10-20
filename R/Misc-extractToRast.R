
#' Extract to raster
#'
#' Extract values from a spatial data source for each cell of a template raster.
#' Resampling is done using "mode" resampling to preserve categorical values.
#'
#' @param input terra SpatRaster, one or more raster files, or sf polygons.
#' @param templateRast SpatRaster. Raster template.
#' @param index numeric or character. Raster layer or vector field to extract.
#' @param crop logical. Crop input before alignment to template raster.
#'
#' @export
#' @return vector with a value for each cell of `templateRast`.
#' Data type matches input data type.
extractToRast <- function(input, templateRast, index = 1, crop = TRUE){

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
    extractToRast_vect(input, templateRast, field = index, crop = TRUE)

  }else{
    extractToRast_rast(input, templateRast, layer = index, crop = TRUE)
  }
}

# Extract values from spatial data source: raster
extractToRast_rast <- function(input, templateRast, layer = 1, crop = TRUE){

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

  # Crop
  if (crop){
    input <- terra::crop(
      input,
      terra::project(terra::as.polygons(templateRast, extent = TRUE), terra::crs(input)),
      snap = "out")
  }

  # Set instructions
  reproject <- !terra::compareGeom(
    input, templateRast,
    crs = TRUE, warncrs = FALSE, stopOnError = FALSE, messages = FALSE,
    lyrs = FALSE, ext = FALSE, rowcol = FALSE, res = FALSE)

  disagg <- !reproject && !terra::is.lonlat(templateRast) &&
    terra::xmin(input) == terra::xmin(templateRast) &&
    terra::ymax(input) == terra::ymax(templateRast) &&
    length(unique(terra::res(input))        == 1) &&
    length(unique(terra::res(templateRast)) == 1) &&
    terra::res(input)[[1]] > terra::res(templateRast)[[1]] &&
    terra::res(input)[[1]] %% terra::res(templateRast)[[1]] == 0

  if (disagg){

    # Disaggregate cells
    input <- terra::disagg(input, fact = terra::res(input)[[1]] / terra::res(templateRast)[[1]])
    input <- terra::crop(input, templateRast)

  }else{

    # Reclassify if contains NAs
    anyNA <- terra::global(input, "anyNA")[1,1]
    if (anyNA){

      valUq <- terra::unique(input, na.rm = FALSE)
      if (length(valUq[,1]) == 1){
        return(rep(valUq[1,1], terra::ncell(templateRast)))
      }

      valUq <- data.table(inp = rev(valUq[,1]), temp = 1:nrow(valUq))
      if (!is.null(cats)) valUq$inp <- match(valUq$inp, cats[[2]])
      input <- terra::classify(input, valUq)
    }

    # Reproject and resample
    if (reproject){
      input <- terra::project(input, templateRast, method = "mode")

    }else{
      terra::crs(input) <- terra::crs(templateRast)
      input <- exactextractr::exact_resample(input, templateRast, fun = "mode")
    }
  }

  # Extract and return raster values
  alignVals <- terra::values(input, mat = FALSE)
  if (!disagg && anyNA) alignVals <- valUq$inp[alignVals]
  if (!is.null(cats)){
    alignVals <- factor(
      cats[match(alignVals, cats[[1]]), 2],
      levels = na.omit(cats[,2]))
  }
  alignVals

}

# Extract values from spatial data source: vector
extractToRast_vect <- function(input, templateRast, field = 1, crop = TRUE){

  reproject <- !terra::compareGeom(
    terra::rast(crs = terra::crs(input)), templateRast,
    crs = TRUE, warncrs = FALSE, stopOnError = FALSE, messages = FALSE,
    lyrs = FALSE, ext = FALSE, rowcol = FALSE, res = FALSE)

  # Crop
  if (crop){
    cropBBOX <- sf::st_as_sfc(sf::st_bbox(templateRast)) |>
      sf::st_buffer(terra::res(templateRast)[[1]], joinStyle = "MITRE", mitreLimit = 2)
    if (reproject){
      cropBBOX <- cropBBOX |>
        sf::st_segmentize(10000) |>
        sf::st_transform(sf::st_crs(input)) |>
        sf::st_bbox() |>
        sf::st_as_sfc()
    }
    suppressWarnings(sf::st_crs(cropBBOX) <- sf::st_crs(input))

    int <- sapply(sf::st_intersects(input, cropBBOX), length) == 1
    if (any(!int)) input <- input[int,]
    rm(int)
  }

  # Reproject
  if (reproject){
    geometry <- NULL # global variable binding
    sf::st_geometry(input) <- sf::st_transform(sf::st_geometry(input), sf::st_crs(templateRast))
  }

  # Dissolve polygons
  if (any(duplicated(input[[field]]))){
    geometry <- NULL # global variable binding
    input <- dplyr::summarise(input, geometry = sf::st_union(geometry), .by = dplyr::all_of(field))
  }

  # Rasterize
  cellIdxRast <- exactextractr::rasterize_polygons(
    input, templateRast, min_coverage = 0.5)

  # Return values
  alignVals <- input[[field]][terra::values(cellIdxRast)]
  if (is.character(alignVals)){
    alignVals <- factor(alignVals, levels = sort(unique(input[[field]])))
  }
  alignVals
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

