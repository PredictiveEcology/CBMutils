utils::globalVariables(c(
  "carbon", "N", "pool",
  "AGlive", "BGlive", "Snags", "Soil"
))

#' `plotPoolProportions`
#'
#' @param pools data.table. Table of pools for each cohort, pixel, or year.
#' Must include the 'year' column.
#'
#' @return `ggplot`
#'
#' @export
#' @importFrom data.table as.data.table is.data.table melt.data.table
#' @importFrom ggplot2 aes element_text element_line geom_area ggplot ggtitle
#' margin scale_x_continuous scale_y_continuous theme
#' @importFrom scales percent
plotPoolProportions <- function(pools){

  if (!"year" %in% names(pools)) stop("pools requires column 'year'")

  if (!identical(names(pools), c("year", "Soil", "BGlive", "AGlive", "Snags"))){

    if (!is.data.table(pools)) pools <- as.data.table(pools)
    pools <- pools[, .(
      Soil   = sum(AboveGroundVeryFastSoil, BelowGroundVeryFastSoil,
                   AboveGroundFastSoil, BelowGroundFastSoil,
                   AboveGroundSlowSoil, BelowGroundSlowSoil, MediumSoil),
      BGlive = sum(CoarseRoots, FineRoots),
      AGlive = sum(Merch, Foliage, Other),
      Snags  = sum(StemSnag, BranchSnag)
    ), by = "year"]
  }

  poolsSum <- data.table::melt.data.table(
    pools, id.vars = "year", variable.name = "pool", value.name = "carbon")
  poolsSum[, proportion := carbon / sum(carbon), by = "year"]

  startYear <- min(poolsSum$year[poolsSum$year != 0])
  if (any(poolsSum$year == 0)){
    poolsSum[year == 0, year := startYear - 1]
  }

  poolsSum$pool <- factor(poolsSum$pool, levels = rev(c("Soil", "BGlive", "AGlive", "Snags")))

  ggplot(data = poolsSum, aes(x = year, y = proportion, fill = pool)) +
    geom_area(color = "grey40", linewidth = .5, alpha = .5) +
    ggtitle("Proportion of C in above and below ground compartments") +
    theme(
      axis.title.x        = element_blank(),
      axis.title.y         = element_blank(),
      axis.text.x          = element_text(angle = 45, vjust = 1, hjust = 1),
      panel.grid.major.x   = element_line(color = "white", linewidth = 0.1),
      panel.grid.minor.x   = element_line(color = "white", linewidth = 0.1),
      legend.position      = "top",
      legend.justification = "left",
      legend.margin        = margin(0, 0, 0, 0),
      legend.title         = element_blank()
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = round(seq(startYear, max(poolsSum$year), length.out = 10)),
      minor_breaks = poolsSum$year
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(0, 1, by = 0.1),
      labels = scales::percent
    )
}


#' `simPlotPoolProportions`
#'
#' @template simCBM
#' @inheritParams cbm4PlotPoolProportions
#' @inheritParams simCBMdbReadSummary
#' @inherit plotPoolProportions description return
#' @export
simPlotPoolProportions <- function(simCBM, years = NULL, cbm4_results = NULL, useCache = TRUE){

  if (!is.null(simCBM$CBM4data)){

    cbm4PlotPoolProportions(
      if (is.null(cbm4_results)) simCBM$CBM4data else cbm4_results,
      year1 = SpaDES.core::start(simCBM),
      years = years
    )

  }else{

    plotPoolProportions(
      simCBMdbReadSummary(
        simCBM, "poolTypes", units = "t/ha", by = "year",
        years = years, useCache = useCache)
    )
  }
}


#' `cbm4PlotPoolProportions`
#'
#' @template cbm4_results
#' @param years numeric. Simulation years to include in plot. Defaults to all simulation years.
#' @param year1 integer. Simulation start year.
#'
#' @inherit plotPoolProportions description return
#' @export
cbm4PlotPoolProportions <- function(cbm4_results, years = NULL, year1 = 1){

  if (length(find.package("CBM4r", quiet = TRUE)) == 0) stop("CBM4r package required")

  cbm4Summary <- CBM4r::cbm4_results_pools_by_timestep(
    cbm4_results, units = "t",
    timesteps = if (!is.null(years)) years - year1 + 1
  )
  cbm4Summary[, year := as.integer(timestep + year1 - 1)]
  data.table::setkey(cbm4Summary, year)
  data.table::setcolorder(cbm4Summary)

  plotPoolProportions(cbm4Summary)
}



