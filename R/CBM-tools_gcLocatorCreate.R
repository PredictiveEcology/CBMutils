#' Create gcLocator raster layer
#'
#' Builds the gcLocator raster from site productivity, spatial unit, and leading species matched to
#' a growth curve lookup table
#'
#' @param siteProductivity Productivity class raster layer
#' @param spuRaster spatial unit ID raster layer
#' @param leadSpecies leading species raster layer
#' @param gcidLookup Table with metadata for each gcid
#'
#'
#' @return `gcLocator` SpatRaster
#'
#' @export
#' @importFrom data.table data.table
gcLocatorCreate <- function(siteProductivity, spuRaster, leadSpecies, gcidLookup) {

  #check if all rasters share the same CRS
  if (!length(unique(lapply(list(siteProductivity, spuRaster, leadSpecies), crs))) == 1){
    stop("Rasters do not all share the same CRS.")
  }

  #build gcTable from the raster layers
  combine <- c(leadSpecies, spuRaster, siteProductivity)
  gcTable <- as.data.table(combine, na.rm = FALSE)
  setnames(gcTable, c("speciesID", "spatialunit", "prodclass"))
  gcTable[, cell := 1:.N]

  #merge gcTable with gcidLookup to get growth curve information for each pixel
  gcTable <- merge(gcTable, gcidLookup, by = c("speciesId", "spatialunit", "prodclass"), all.x = TRUE)
  colstokeep <- c("cell", "growthcurveid")
  gcTable <- gcTable[, ..colstokeep]
  setnames(gcTable, c("cell", "growthcurveid"))

  #build the gcLocator raster layer
  gcLocator <- rast(leadSpecies)
  values(gcLocator) <- NA
  values(gcLocator)[gcTable$cell] <- gcTable$growthcurveid

  return(gcLocator)

}
