utils::globalVariables(c(
  "totalCarbon"
))

#' `mapTotalCarbon`
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
#' @importFrom ggplot2 coord_sf ggplot ggtitle labs scale_fill_gradient2 scale_x_continuous scale_y_continuous
#' @importFrom tidyterra geom_spatraster
#' @importFrom terra rast unwrap
mapTotalCarbon <- function(pools, masterRaster, year = NULL){

  if (!"pixelIndex" %in% names(pools)) stop("pools requires column 'pixelIndex'")

  if (is.null(masterRaster)) stop("masterRaster not found")
  masterRaster <- terra::unwrap(terra::rast(masterRaster))

  # Calculate total carbon for each pixel
  if (!identical(names(pools), c("pixelIndex", "totalCarbon"))){

    if (!is.data.table(pools)) pools <- as.data.table(pools)
    pools <- pools[, .(
      pixelIndex,
      totalCarbon = rowSums(pools[, .(
        Merch, Foliage, Other, CoarseRoots, FineRoots,
        AboveGroundVeryFastSoil, BelowGroundVeryFastSoil, AboveGroundFastSoil,
        BelowGroundFastSoil, MediumSoil, AboveGroundSlowSoil, BelowGroundSlowSoil,
        StemSnag, BranchSnag
      )])
    )][, lapply(.SD, sum), by = "pixelIndex"]
  }

  # Plot
  plotTitle <- "Total Carbon"
  if (!is.null(year)) plotTitle <- paste(plotTitle, "in", year)

  withr::local_envvar(tidyterra.quiet = TRUE)

  plotRast <- terra::rast(
    res  = 1,
    xmin = 0, xmax = terra::ncol(masterRaster),
    ymin = 0, ymax = terra::nrow(masterRaster)
  )
  terra::set.values(plotRast, pools$pixelIndex, round(pools$totalCarbon))

  ggplot() +
    tidyterra::geom_spatraster(data = plotRast) +
    coord_sf() +
    theme_no_axes() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradient2(
      low = "#D73027",       # red for low
      mid = "#FEE08B",       # yellow for middle
      high = "#1A9850",      # green for high
      midpoint = mean(pools$totalCarbon, na.rm = TRUE),
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

  mapTotalCarbon(
    spadesCBMdbReadSummary(
      spadesCBMdb, "totalCarbon", units = "t/ha", by = "pixelIndex",
      year = year, useCache = useCache),
    masterRaster = masterRaster,
    year = year
  )
}




