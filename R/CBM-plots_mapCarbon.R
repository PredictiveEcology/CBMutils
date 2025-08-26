utils::globalVariables(c(
  "Merch", "Foliage", "Other", "CoarseRoots", "FineRoots",
  "AboveGroundVeryFastSoil", "BelowGroundVeryFastSoil", "AboveGroundFastSoil",
  "BelowGroundFastSoil", "MediumSoil", "AboveGroundSlowSoil", "BelowGroundSlowSoil",
  "StemSnag", "BranchSnag",
  "totalCarbon", "x", "y"
))

#' `mapCarbon`
#'
#' Map total carbon across a study area.
#'
#' @param pools data.table. Table of pools for each cohort or for each pixel.
#' Must include the 'pixelIndex' column.
#' @template masterRaster
#' @param year numeric. Year that the map represents.
#' If provided, it will be included in the plot title.
#'
#' @return `ggplot`
#'
#' @export
#' @importFrom data.table as.data.table is.data.table
#' @importFrom ggforce theme_no_axes
#' @importFrom ggplot2 aes geom_raster ggplot ggtitle scale_fill_continuous coord_fixed
#' @importFrom terra rast unwrap xyFromCell
mapCarbon <- function(pools, masterRaster, year = NULL){

  if (!"pixelIndex" %in% names(pools)) stop("pools requires column 'pixelIndex'")
  if (is.null(masterRaster)) stop("masterRaster not found")

  # Calculate total carbon for each pixel
  if (!identical(names(pools), c("pixelIndex", "totalCarbon"))){

    if (!is.data.table(pools)) pools <- as.data.table(pools)
    pools <- pools[, .(
      totalCarbon = sum(
        Merch, Foliage, Other, CoarseRoots, FineRoots,
        AboveGroundVeryFastSoil, BelowGroundVeryFastSoil, AboveGroundFastSoil,
        BelowGroundFastSoil, MediumSoil, AboveGroundSlowSoil, BelowGroundSlowSoil,
        StemSnag, BranchSnag)
    ), by = "pixelIndex"]
  }

  # Plot
  plotTitle <- "Total Carbon"
  if (!is.null(year)) plotTitle <- paste(plotTitle, "in", year)

  pools <- cbind(pools, terra::xyFromCell(terra::unwrap(terra::rast(masterRaster)), pools$pixelIndex))

  ggplot() + geom_raster(data = pools, aes(x = x, y = y, fill = totalCarbon)) +
    theme_no_axes() + coord_fixed() +
    scale_fill_continuous(low = "#873f38", high = "#61d464", na.value = "transparent", guide = "colorbar") +
    labs(fill = "Carbon (MgC/ha)" ) +
    ggtitle(plotTitle)
}


#' `simMapCarbon`
#'
#' @inheritParams simCBMdbReadSummary
#' @inherit mapCarbon description return
#' @export
simMapCarbon <- function(simCBM, year, useCache = TRUE){

  if (missing(year)){
    year <- SpaDES.core::convertTimeunit(SpaDES.core::times(simCBM)$current, "year")
  }

  mapCarbon(
    simCBMdbReadSummary(
      simCBM, "totalCarbon", by = "pixelIndex",
      year = year, useCache = useCache),
    year = year,
    masterRaster = simCBM$masterRaster
  )
}

