utils::globalVariables(c(
  "AboveGroundFastSoil", "AboveGroundSlowSoil", "AboveGroundVeryFastSoil",
  "BelowGroundFastSoil", "BelowGroundSlowSoil", "BelowGroundVeryFastSoil",
  "BranchSnag", "CoarseRoots", "FineRoots", "Foliage", "MediumSoil", "Merch", "Other", "StemSnag",
  "AGlive", "BGlive", "carbon", "cohortGroupID", "N", "pool", "simYear", "snags", "soil", "weight"
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
#' @inheritParams simCBMdbReadSummary
#' @param years numeric. Simulation years to include in plot. Defaults to all simulation years.
#' @inherit plotPoolProportions description return
#' @export
simPlotPoolProportions <- function(simCBM, years = NULL, useCache = TRUE){

  plotPoolProportions(
    simCBMdbReadSummary(
      simCBM, "poolTypes", by = "year",
      years = years, useCache = useCache)
  )
}
