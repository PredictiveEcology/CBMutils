#' `prepInputsEcozones`
#'
#' @param url A url to the data
#' @param dPath destination path
#' @param rasterToMatch A raster with NAs reprsenting "off study area" and otherwise
#'   any arbitrary value for "in study area".
#'   Equivalent to \code{rasterToMatch} argument in \code{\link[reproducible]{prepInputs}}.
#'
#' @export
#' @importFrom reproducible prepInputs
#' @importFrom sf st_transform st_crop st_crs st_as_sf
#' @importFrom fasterize fasterize
prepInputsEcozones <- function(url, dPath, rasterToMatch) {
    ecozones <- prepInputs(
    # this website https://sis.agr.gc.ca/cansis/index.html is hosted by the Canadian Government
    url = url,
    alsoExtract = "similar",
    destinationPath = dPath,
    # rasterToMatch = rasterToMatch,
    # studyArea = sim$studyArea,
    # useSAcrs = TRUE,
    overwrite = TRUE,
    fun = "sf::st_read",
    filename2 = TRUE
  )
  ecozones <- st_transform(ecozones, st_crs(rasterToMatch))
  ecozones <- st_crop(ecozones, st_as_sf(as(extent(rasterToMatch), "SpatialPolygons")))
  ecozones <- ecozones[!ecozones$ZONE_NAME %in% "Pacific Maritime",]
  ecozoneRaster <- fasterize::fasterize(ecozones, rasterToMatch, field = "ECOZONE")
  ecozoneRaster[is.na(rasterToMatch[])] <- NA
  ecozoneRaster
}

#' `prepInputsVRI`
#'
#' Read in the BC VRI, with growth curve information (from `ws3`), and creates a raster stack of
#' the age and `gcID`.
#'
#' @inheritParams prepInputsEcozones
#' @export
#' @importFrom fasterize fasterize
#' @importFrom raster stack
#' @importFrom sf st_read st_transform
prepInputsVRI <- function(url, dPath, rasterToMatch) {
  VRIin <- prepInputs(url = url,
                      fun = "sf::st_read",
                      destinationPath = dPath)
  RIA_VRI <- st_transform(VRIin, crs = st_crs(rasterToMatch))
  gcIDRaster <- fasterize::fasterize(RIA_VRI, rasterToMatch, field = "curve2")
  ageRaster <- fasterize::fasterize(RIA_VRI, rasterToMatch, field = "PROJ_AGE_1")
  gcIDRaster[] <- as.integer(gcIDRaster[])
  ageRaster[] <- as.integer(ageRaster[])
  VRIraster <- raster::stack(gcIDRaster, ageRaster)
  return(VRIraster)
}