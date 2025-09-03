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
#' @importFrom ggplot2 aes expansion geom_col ggplot ggtitle guides guide_legend labs
#' scale_fill_brewer scale_fill_discrete scale_y_continuous theme_classic
plotPoolProportions <- function(pools){

  if (!"year" %in% names(pools)) stop("pools requires column 'year'")

  if (!identical(names(pools), c("year", "soil", "AGlive", "BGlive", "snags"))){

    if (!is.data.table(pools)) pools <- as.data.table(pools)
    pools <- pools[, .(
      soil  = sum(AboveGroundVeryFastSoil, BelowGroundVeryFastSoil,
                  AboveGroundFastSoil, BelowGroundFastSoil,
                  AboveGroundSlowSoil, BelowGroundSlowSoil, MediumSoil),
      AGlive = sum(Merch, Foliage, Other),
      BGlive = sum(CoarseRoots, FineRoots),
      snags  = sum(StemSnag, BranchSnag)
    ), by = "year"]
  }

  pools <- data.table::melt.data.table(
    pools, id.vars = "year", variable.name = "pool", value.name = "carbon")

  pools[, proportion := carbon / sum(carbon), by = "year"]

  ggplot(data = pools, aes(x = year, y = proportion, fill = pool)) +
    geom_col(position = "fill") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    labs(x = "Year", y = "Proportion") + theme_classic() + ggtitle("Proportion of C above and below ground compartments.") +
    guides(fill = guide_legend(title.position= "top", title ="Carbon compartment") ) +
    scale_fill_brewer(palette = "Set1", labels = c("Soil", "AGlive", "BGlive", "snags"))
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
