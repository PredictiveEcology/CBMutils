utils::globalVariables(c(
  "emission", "emissionType", "Products", "year"
))

#' `plotEmissionsProducts`
#'
#' Plot yearly emissions and products.
#'
#' @param emissionsProducts TODO
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

  Emissions <- ggplot(data = outTable, aes(x = year, y = emission, fill = emissionType)) +
    geom_bar(stat = "identity") +
    labs(x = "Year", y = "Carbon in MgC") + theme_classic() + ggtitle("Yearly Emissions") +
    guides(fill = guide_legend(title.position= "top", title ="Emission") ) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values = c("#733958", "#5c538a", "#c3a44c"), labels = c('CO2', "CH4", "CO"))

  Products <- ggplot(data = emissionsProducts, aes(x = year, y = Products)) +
    geom_bar(stat = "identity", fill = "#4e88b9") +
    labs(x = "Year", y = "Carbon in MgC") + theme_classic() + ggtitle("Yearly Products") +
    scale_y_continuous(expand = c(0,0))

  plot_grid(Emissions, Products, ncol = 2)
}


#' `simPlotEmissionsProducts`
#'
#' @template simCBM
#' @inherit plotEmissionsProducts description return
#' @export
simPlotEmissionsProducts <- function(simCBM){
  plotEmissionsProducts(simCBM$emissionsProducts)
}

