utils::globalVariables(c(
  "DeltaBiomass_AG", "DeltaBiomass_BG",
  "TurnoverMerchLitterInput", "TurnoverFolLitterInput",
  "TurnoverOthLitterInput", "TurnoverCoarseLitterInput", "TurnoverFineLitterInput",
  "NPP", "x", "y"
))

#' `mapNPP`
#'
#' Map net primary productivity (NPP) across a study area.
#'
#' @param flux data.table. Table of flux for each cohort or for each pixel.
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
#' @importFrom ggplot2 ggplot geom_raster aes scale_fill_continuous ggtitle coord_fixed
#' @importFrom terra rast res unwrap values
mapNPP <- function(flux, masterRaster, year = NULL) {

  if (!"pixelIndex" %in% names(flux)) stop("flux requires column 'pixelIndex'")
  if (is.null(masterRaster)) stop("masterRaster not found")

  # Calculate NPP
  if (!identical(names(flux), c("pixelIndex", "NPP"))){

    if (!is.data.table(flux)) flux <- as.data.table(flux)
    flux <- flux[, .(
      NPP = sum(
        DeltaBiomass_AG, DeltaBiomass_BG,
        TurnoverMerchLitterInput, TurnoverFolLitterInput,
        TurnoverOthLitterInput, TurnoverCoarseLitterInput, TurnoverFineLitterInput)
    ), by = "pixelIndex"]
  }

  # Plot
  plotTitle <- "Net Primary Productivity (NPP)"
  if (!is.null(year)) plotTitle <- paste(plotTitle, "in", year)

  flux <- cbind(flux, terra::xyFromCell(terra::unwrap(terra::rast(masterRaster)), flux$pixelIndex))

  ggplot() + geom_raster(data = flux, aes(x = x, y = y, fill = NPP)) +
    theme_no_axes() + coord_fixed() +
    scale_fill_continuous(low = "#873f38", high = "#61d464", na.value = "transparent", guide = "colorbar") +
    labs(fill = "Carbon (MgC/ha)" ) +
    ggtitle(paste(plotTitle, "\n", "Landscape average:", round(mean(flux$NPP), 3), "MgC/ha."))
}


#' `simMapNPP`
#'
#' @inheritParams simCBMdbReadSummary
#' @inherit mapNPP description return
#' @export
simMapNPP <- function(simCBM, year, useCache = TRUE){

  if (missing(year)){
    year <- SpaDES.core::convertTimeunit(SpaDES.core::times(simCBM)$current, "year")
  }

  mapNPP(
    simCBMdbReadSummary(
      simCBM, "NPP", by = "pixelIndex",
      year = year, useCache = useCache),
    year = year,
    masterRaster = simCBM$masterRaster
  )
}

