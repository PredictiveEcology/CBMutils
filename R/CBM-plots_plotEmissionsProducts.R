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
#' @importFrom ggplot2 aes element_text geom_bar geom_col geom_line ggplot guides guide_legend labs  scale_fill_manual scale_x_continuous scale_y_continuous sec_axis theme theme_classic xlab
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


#' CBM4: `plotEmissionsProducts`
#'
#' @template cbm4_results
#' @param years integer. Year(s) of simulation results.
#' @param yearStart integer. Simulation start year.
#'
#' @inherit mapTotalCarbon description return
#' @export
cbm4PlotEmissionsProducts <- function(cbm4_results, years = NULL, yearStart = 1){

  if (length(find.package("CBM4r", quiet = TRUE)) == 0) stop("CBM4r package required")

  timesteps <- if (!is.null(years)) years - yearStart + 1

  cbm4_results <- CBM4r::cbm4_results_processor(cbm4_results)

  cbm4_totals <- merge(
    CBM4r::cbm4_results_totals(
      cbm4_results,
      timesteps    = timesteps,
      view_name    = "composite_flux_indicators",
      view_columns = c(
        "CH4" = "Emissions - Emissions By Gas - Total CH4",
        "CO"  = "Emissions - Emissions By Gas - Total CO",
        "CO2" = "Emissions - Emissions By Gas - Total CO2"
      )),
    CBM4r::cbm4_results_totals(
      cbm4_results,
      timesteps    = timesteps,
      view_name    = "composite_disturbance_indicators",
      view_columns = c(
        "Products" = "Ecosystem Transfers - Ecosystem to Forest Products - Total Harvest (Biomass + Snags)"
      )),
    all = TRUE)[, .(
      year      = timestep + yearStart - 1,
      Products  = data.table::fcoalesce(Products, 0),
      Emissions = CO2 + CH4 + CO,
      CO2       = CO2,
      CH4       = CH4,
      CO        = CO
    )]

  plotEmissionsProducts(cbm4_totals)
}


#' `simPlotEmissionsProducts`
#'
#' @template simCBM
#' @param years numeric. Simulation years to include in plot. Defaults to all simulation years.
#' @inheritParams spadesCBMdbReadSummary
#' @inherit plotEmissionsProducts description return
#' @export
simPlotEmissionsProducts <- function(simCBM, years = NULL, useCache = TRUE){

  if ("emissionsProducts" %in% names(simCBM)){

    emissionsProducts <- simCBM$emissionsProducts
    if (!is.null(years)) emissionsProducts <- subset(emissionsProducts, year %in% years)
    plotEmissionsProducts(emissionsProducts)

  }else if (!is.null(simCBM$CBM4data)){

    cbm4PlotEmissionsProducts(
      simCBM$CBM4data,
      years     = years,
      yearStart = simYears(simCBM)$start
    )

  }else{

    if (is.null(years)) years <- with(simYears(simCBM), start:end)

    spadesCBMdbPlotEmissionsProducts(
      simCBM$spadesCBMdb,
      years    = years,
      useCache = useCache
    )
  }
}


#' spadesCBMdb `plotEmissionsProducts`
#'
#' @inheritParams spadesCBMdbReadSummary
#' @param years numeric. Simulation years to include in plot.
#' @inherit plotEmissionsProducts description return
#' @export
spadesCBMdbPlotEmissionsProducts <- function(spadesCBMdb, years, useCache = TRUE){

  emissionsProducts <- merge(
    spadesCBMdbReadSummary(
      spadesCBMdb, "products", by = "year", units = "t",
      years = min(years):max(years), useCache = useCache),
    spadesCBMdbReadSummary(
      spadesCBMdb, "emissions", by = "year", units = "t",
      years = min(years):max(years), useCache = useCache),
    by = "year", all = TRUE)

  # Summarize yearly (non-cumulative) products
  for (i in setdiff(1:nrow(emissionsProducts), 1)){
    emissionsProducts$Products[[i]] <- emissionsProducts$Products[[i]] - sum(emissionsProducts$Products[1:(i - 1)])
  }
  emissionsProducts <- subset(emissionsProducts, year %in% years)

  plotEmissionsProducts(emissionsProducts)
}



