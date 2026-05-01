utils::globalVariables(c(
  "totalCarbon"
))

#' `mapTotalCarbon`
#'
#' Map total carbon across a study area.
#'
#' @param rastTC SpatRaster. Pixel values represent total carbon (t/ha).
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
mapTotalCarbon <- function(rastTC, year = NULL){

  withr::local_envvar(tidyterra.quiet = TRUE)

  plotTitle <- "Total Carbon"
  if (!is.null(year)) plotTitle <- paste(plotTitle, "in", year)

  ggplot() +
    tidyterra::geom_spatraster(data = rastTC) +
    coord_sf() +
    theme_no_axes() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradient2(
      low = "#D73027",       # red for low
      mid = "#FEE08B",       # yellow for middle
      high = "#1A9850",      # green for high
      midpoint = terra::global(rastTC, "mean", na.rm = TRUE)[[1]],
      na.value = "transparent",
      guide = "colorbar"
    ) +
    labs(fill = "Carbon\n(t/ha)") +
    ggtitle(plotTitle)
}


#' `simMapTotalCarbon`
#'
#' @template simCBM
#' @inheritParams simCBMdbReadSummary
#' @param year numeric. Year of simulation results. Defaults to the current simulation year.
#' @inherit mapTotalCarbon description return
#' @export
simMapTotalCarbon <- function(simCBM, year = NULL, useCache = TRUE){

  if (is.null(year)){
    year <- SpaDES.core::convertTimeunit(SpaDES.core::times(simCBM)$current, "year")
  }

  spadesCBMdbMapTotalCarbon(
    simCBM$spadesCBMdb,
    masterRaster = simCBM$masterRaster,
    year         = year,
    useCache     = useCache
  )
}


#' spadesCBMdb `simMapTotalCarbon`
#'
#' @inheritParams spadesCBMdbReadSummary
#' @template masterRaster
#' @param year numeric. Year of simulation results.
#' @inherit mapTotalCarbon description return
#' @export
spadesCBMdbMapTotalCarbon <- function(spadesCBMdb, masterRaster, year, useCache = TRUE){

  tblTC <- spadesCBMdbReadSummary(
    spadesCBMdb, "totalCarbon", units = "t/ha", by = "pixelIndex",
    year = year, useCache = useCache)

  rastTC <- terra::rast(
    res  = 1,
    xmin = 0, xmax = terra::ncol(masterRaster),
    ymin = 0, ymax = terra::nrow(masterRaster)
  )
  terra::set.values(rastTC, tblTC$pixelIndex, tblTC$totalCarbon)

  rm(tblTC)

  mapTotalCarbon(rastTC, year = year)
}




