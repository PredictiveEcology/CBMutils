
#' Write raster with values
#'
#' Replace the values in a `SpatRaster` before writing it to file.
#'
#' @param templateRast terra `SpatRaster` template defining raster geometry.
#' @param filename character. Output filename.
#' @param cells integer. Raster cell index.
#' @param values numeric or character. New raster values.
#' @param ... arguments to \code{\link[terra]{writeRaster}}
#'
#' @export
writeRasterWithValues <- function(templateRast, filename, cells = NULL, values = NULL, ...){

  templateRast <- terra::rast(templateRast)

  if (!is.null(values)){

    if (!is.numeric(values)){
      values <- factor(values)
      cats <- unique(data.frame(
        value    = as.integer(values),
        category = as.character(values)
      ))
      cats <- cats[cats$value,]
    }

    if (is.null(cells)){
      templateRast <- terra::rast(templateRast, vals = values)
    }else{
      templateRast <- terra::rast(templateRast)
      terra::set.values(templateRast, cells = cells, values = values)
    }

    if (!is.numeric(values)) terra::set.cats(templateRast, layer = 1, cats)
  }

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(templateRast, filename = filename, ...)

  return(invisible())
}

