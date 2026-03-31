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


#' `cbm4MapTotalCarbon`
#'
#' @template simCBM
#' @inheritParams cbm4MapTotalCarbon
#' @inheritParams simCBMdbReadSummary
#' @inherit mapTotalCarbon description return
#' @export
simMapTotalCarbon <- function(simCBM, year, cbm4_results = NULL, useCache = TRUE){

  if (missing(year)){
    year <- SpaDES.core::convertTimeunit(SpaDES.core::times(simCBM)$current, "year")
  }

  if (!is.null(simCBM$CBM4data)){

    cbm4MapTotalCarbon(
      if (is.null(cbm4_results)) simCBM$CBM4data else cbm4_results,
      year1 = SpaDES.core::start(simCBM),
      year  = year
    )

  }else{

    mapTotalCarbon(
      simCBMdbReadSummary(
        simCBM, "totalCarbon", units = "t/ha", by = "pixelIndex",
        year = year, useCache = useCache),
      year = year,
      masterRaster = simCBM$masterRaster
    )
  }
}


#' `cbm4MapTotalCarbon`
#'
#' @template cbm4_results
#' @param year numeric. Year of simulation results.
#' @param year1 integer. Simulation start year.
#'
#' @inherit mapTotalCarbon description return
#' @export
cbm4MapTotalCarbon <- function(cbm4_results, year, year1 = 1){

  if (length(find.package("CBM4r", quiet = TRUE)) == 0) stop("CBM4r package required")

  cbm4Summary <- CBM4r::cbm4_results_pools_by_pixel(
    cbm4_results, units = "t/ha",
    timestep = year - year1 + 1
  )
  data.table::setnames(cbm4Summary, "pixel_index", "pixelIndex")

  mapTotalCarbon(cbm4Summary, masterRaster = CBM4r::cbm4_read_geo(cbm4_results), year = year)
}



