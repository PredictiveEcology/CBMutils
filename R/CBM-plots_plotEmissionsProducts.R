utils::globalVariables(c(
  "emission", "emissionType"
))

#' `plotEmissionsProducts`
#'
#' Plot yearly emissions and products.
#'
#' @param emissionsProducts Table of simlation emissions and products by cohort group by year
#'
#' @return `ggplot`
#'
#' @export
#' @importFrom cowplot plot_grid
#' @importFrom data.table as.data.table is.data.table melt.data.table
#' @importFrom ggplot2 aes element_text geom_bar geom_col geom_line ggplot
#' guides guide_legend labs  scale_fill_manual scale_x_continuous scale_y_continuous
#' sec_axis theme theme_classic xlab
#' @importFrom scales pretty_breaks
plotEmissionsProducts <- function(emissionsProducts) {

  if (!is.data.table(emissionsProducts)) emissionsProducts <- as.data.table(emissionsProducts)

  outTable <- data.table::melt.data.table(emissionsProducts, id.vars = "year",
                                          measure.vars = c("CO2", "CH4", "CO"),
                                          variable.name = "emissionType", value.name = "emission")

  Emissions <- ggplot(data = outTable, aes(x = year, y = as.numeric(emission), fill = emissionType)) +
    geom_bar(stat = "identity") + theme_classic() +
    labs(x = "Year", y = "Carbon (t)") + ggtitle("Emissions") +
    guides(fill = guide_legend(title.position = "top", title = "Emissions")) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values = c("#733958", "#5c538a", "#c3a44c"), labels = c('CO2', "CH4", "CO"))

  Products <- ggplot(data = emissionsProducts, aes(x = year, y = as.numeric(Products))) +
    geom_bar(stat = "identity", fill = "#4e88b9") + theme_classic() +
    labs(x = "Year", y = "Carbon (t)") + ggtitle("Products") +
    scale_y_continuous(expand = c(0,0))

  plot_grid(Emissions, Products, ncol = 2)
}


#' `simPlotEmissionsProducts`
#'
#' @template simCBM
#' @inheritParams cbm4PlotPoolProportions
#' @inheritParams simCBMdbReadSummary
#' @inherit plotEmissionsProducts description return
#' @export
simPlotEmissionsProducts <- function(simCBM, years = NULL, cbm4_results = NULL, useCache = TRUE){

  if ("emissionsProducts" %in% names(simCBM)){

    emissionsProducts <- simCBM$emissionsProducts
    if (!is.null(years)) emissionsProducts <- subset(emissionsProducts, year %in% years)
    plotEmissionsProducts(emissionsProducts)

  }else if (!is.null(simCBM$CBM4data)){

    cbm4PlotEmissionsProducts(
      if (is.null(cbm4_results)) simCBM$CBM4data else cbm4_results,
      year1 = SpaDES.core::start(simCBM),
      years = years
    )

  }else{

    emissionsProducts <- merge(
      simCBMdbReadSummary(
        simCBM, "products", by = "year", units = "t",
        years = if (!is.null(years)) min(years):max(years), useCache = useCache),
      simCBMdbReadSummary(
        simCBM, "emissions", by = "year", units = "t",
        years = if (!is.null(years)) min(years):max(years), useCache = useCache),
      by = "year", all = TRUE)

    # Summarize yearly (non-cumulative) products
    for (i in setdiff(nrow(emissionsProducts):1, 1)){
      emissionsProducts$Products[[i]] <- emissionsProducts$Products[[i]] - sum(emissionsProducts$Products[1:(i - 1)])
    }

    if (!is.null(years)) emissionsProducts <- subset(emissionsProducts, year %in% years)
    plotEmissionsProducts(emissionsProducts)
  }
}


#' `cbm4PlotEmissionsProducts`
#'
#' @template cbm4_results
#' @param years numeric. Simulation years to include in plot. Defaults to all simulation years.
#' @param year1 integer. Simulation start year.
#'
#' @inherit plotEmissionsProducts description return
#' @export
cbm4PlotEmissionsProducts <- function(cbm4_results, years = NULL, year1 = 1){

  if (length(find.package("CBM4r", quiet = TRUE)) == 0) stop("CBM4r package required")

  timesteps <- if (!is.null(years)) years - year1 + 1

  emissions <- CBM4r::cbm4_results_emissions_by_timestep(cbm4_results, units = "t", timesteps = timesteps)

  products  <- CBM4r::cbm4_results_pools_by_timestep(
    cbm4_results, units = "t",
    timesteps = if (!is.null(timesteps)) min(timesteps):max(timesteps)
  )[timestep > 0, .(timestep, Products)]

  for (i in setdiff(nrow(products):1, 1)){
    products$Products[[i]] <- products$Products[[i]] - products$Products[[i - 1]]
  }
  if (!is.null(timesteps)) products <- products[timestep %in% timesteps,]

  cbm4Summary <- merge(emissions, products, by = "timestep", all = TRUE)
  cbm4Summary[is.na(cbm4Summary)] <- 0

  cbm4Summary[, year := as.integer(timestep + year1 - 1)]
  data.table::setkey(cbm4Summary, year)
  data.table::setcolorder(cbm4Summary)

  plotEmissionsProducts(cbm4Summary)
}



