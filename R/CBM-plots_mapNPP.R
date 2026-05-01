utils::globalVariables(c(
  "NPP"
))

#' `mapNPP`
#'
#' Map Net Primary Productivity (NPP) across a study area.
#'
#' @param rastNPP SpatRaster. Pixel values represent NPP (t/ha).
#' @param year numeric. Year that the map represents.
#' If provided, it will be included in the plot title.
#'
#' @return `ggplot`
#'
#' @export
#' @importFrom data.table as.data.table is.data.table
#' @importFrom ggforce theme_no_axes
#' @importFrom ggplot2 coord_sf ggplot ggtitle labs scale_fill_gradient2 scale_x_continuous scale_y_continuous
#' @importFrom tidyterra geom_spatraster
#' @importFrom terra rast unwrap
mapNPP <- function(rastNPP, year = NULL) {

  withr::local_envvar(tidyterra.quiet = TRUE)

  plotTitle <- "Net Primary Productivity (NPP)"
  if (!is.null(year)) plotTitle <- paste(plotTitle, "in", year)

  meanNPP <- terra::global(rastNPP, "mean", na.rm = TRUE)[[1]]

  ggplot() +
    tidyterra::geom_spatraster(data = rastNPP) +
    coord_sf() +
    theme_no_axes() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradient2(
      low = "#D73027",       # red for low
      mid = "#FEE08B",       # yellow for middle
      high = "#1A9850",      # green for high
      midpoint = meanNPP,
      na.value = "transparent",
      guide = "colorbar"
    ) +
    labs(fill = "Carbon\n(t/ha)") +
    ggtitle(paste0(plotTitle, "\n", "Landscape average: ", round(meanNPP, 3), " t/ha."))
}


#' `simMapNPP`
#'
#' @template simCBM
#' @inheritParams spadesCBMdbReadSummary
#' @param year numeric. Year of simulation results. Defaults to the current simulation year.
#' @inherit mapNPP description return
#' @export
simMapNPP <- function(simCBM, year, useCache = TRUE){

  if (is.null(year)){
    year <- SpaDES.core::convertTimeunit(SpaDES.core::times(simCBM)$current, "year")
  }

  spadesCBMdbMapNPP(
    simCBM$spadesCBMdb,
    masterRaster = simCBM$masterRaster,
    year         = year,
    useCache     = useCache
  )
}

#' spadesCBMdb: `mapNPP`
#'
#' @inheritParams spadesCBMdbReadSummary
#' @template masterRaster
#' @param year numeric. Year of simulation results.
#' @inherit mapNPP description return
#' @export
spadesCBMdbMapNPP <- function(spadesCBMdb, masterRaster, year, useCache = TRUE){

  tblNPP <- spadesCBMdbReadSummary(
    spadesCBMdb, "NPP", by = "pixelIndex",
    year = year, useCache = useCache)

  rastNPP <- terra::rast(
    res  = 1,
    xmin = 0, xmax = terra::ncol(masterRaster),
    ymin = 0, ymax = terra::nrow(masterRaster)
  )
  terra::set.values(rastNPP, tblNPP$pixelIndex, tblNPP$NPP)

  rm(tblNPP)

  mapNPP(rastNPP, year = year)
}





