utils::globalVariables(c(
  "AboveGroundFastSoil", "AboveGroundSlowSoil", "AboveGroundVeryFastSoil", "AGB", "AGlive",
  "BelowGroundFastSoil", "BelowGroundSlowSoil", "BelowGroundVeryFastSoil", "BGB", "BGlive",
  "BranchSnag", "carbon", "CH4", "CO", "CO2", "CoarseRoots", "cohortGroup", "cohortGroupID",
  "description", "disturbance_matrix_id", "disturbance_type_id",
  "DOM", "emission", "Emissions", "emissionType", "emissionsCH4", "emissionsCO", "emissionsCO2",
  "FineRoots", "Foliage", "HardwoodBranchSnag", "HardwoodStemSnag",
  "locale_id", "MediumSoil", "Merch", "N", "Other","pixelIndex", "pixNPP", "pixTC",
  "pool", "products", "Products",
  "res", "simYear", "snags", "SoftwoodBranchSnag", "SoftwoodStemSnag", "soil", "StemSnag", "weight",
  "x", "y", "avgNPP", "totalCarbon"
))

#' `spatialPlot`
#'
#' @description
#' Maps the total carbon across the landscape for a given year.
#'
#' @param cbmPools TODO
#' @param years TODO
#' @template masterRaster
#' @param cohortGroupKeep TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table as.data.table
#' @importFrom ggforce theme_no_axes
#' @importFrom ggplot2 aes geom_raster ggplot ggtitle scale_fill_continuous coord_fixed
#' @importFrom terra rast res unwrap values
spatialPlot <- function(cbmPools, years, masterRaster, cohortGroupKeep) {
  masterRaster <- terra::unwrap(masterRaster)
  # filter cbmPools to keep the year to plot
  cbmPools <- as.data.table(cbmPools)[simYear == years, ]
  # Calculate the total carbon per cohort group
  totalCarbon <- apply(cbmPools[, Merch:BranchSnag],
                       1, "sum")
  totalCarbon <- cbind(cbmPools, totalCarbon)
  # Get the pixel index for each cohort group for the year to plot
  t <- unique(cohortGroupKeep[, .(pixelIndex, cohortGroupID = get(as.character(years)))])
  setkey(t, cohortGroupID)
  setkey(totalCarbon, cohortGroupID)
  # Match pixel index and carbon data.
  # allow.cartesian: there might be >1 cohort groups per pixel, and >1 pixels per cohort group
  temp <- merge(t, totalCarbon, allow.cartesian = TRUE)
  # Calculate total carbon per pixel
  temp <- temp[, .(totalCarbon = sum(totalCarbon)), by = pixelIndex]
  setkey(temp, pixelIndex)
  # Create the raster to plot
  plotM <- terra::rast(masterRaster)
  names(plotM) <- "totalCarbon"
  terra::values(plotM) <- NA
  terra::values(plotM)[temp$pixelIndex] <- temp$totalCarbon
  # Convert to data.frame to work with geom_raster
  plotM <- terra::as.data.frame(plotM, xy = TRUE)
  pixSize <- prod(terra::res(masterRaster))/10000
  temp[, `:=`(pixTC, totalCarbon * pixSize)]
  overallTC <- sum(temp$pixTC)/(nrow(temp) * pixSize)
  Plot <- ggplot() + geom_raster(data = plotM, aes(x = x, y = y, fill = totalCarbon)) +
    theme_no_axes() + scale_fill_continuous(low = "#873f38", high = "#61d464", na.value = "transparent", guide = "colorbar") + labs(fill = "Carbon (MgC)" ) +
    ggtitle(paste0("Total Carbon in ", years, " in MgC/ha")) + coord_fixed()
}

#' `carbonOutPlot`
#'
#' @param emissionsProducts TODO
#'
#' @return invoked for side effect of creating plot
#'
#' @export
#' @importFrom cowplot plot_grid
#' @importFrom data.table as.data.table melt.data.table
#' @importFrom ggplot2 aes element_text geom_bar geom_col geom_line ggplot
#' guides labs  scale_fill_manual scale_x_continuous scale_y_continuous
#' sec_axis theme theme_classic xlab
#' @importFrom scales pretty_breaks
carbonOutPlot <- function(emissionsProducts) {
  totalOutByYr <- as.data.table(emissionsProducts)
  outTable <- data.table::melt.data.table(totalOutByYr, id.vars = "simYear",
                                          measure.vars = c("CO2", "CH4", "CO"),
                                          variable.name = "emissionType", value.name = "emission")

  Emissions <- ggplot(data = outTable, aes(x = simYear, y = emission, fill = emissionType)) +
    geom_bar(stat = "identity") +
    labs(x = "Year", y = "Carbon in MgC") + theme_classic() + ggtitle("Yearly Emissions") +
    guides(fill = guide_legend(title.position= "top", title ="Emission") ) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values = c("#733958", "#4e88b9", "#c3a44c"), labels = c('CO2', "CH4", "CO"))

  Products <- ggplot(data = totalOutByYr, aes(x = simYear, y = Products)) +
    geom_bar(stat = "identity", fill = "#4e88b9") +
    labs(x = "Year", y = "Carbon in MgC") + theme_classic() + ggtitle("Yearly Products") +
    scale_y_continuous(expand = c(0,0))

  plot_grid(Emissions, Products, ncol = 2)
}

#' `NPPplot`
#'
#' @description
#' Maps the average net primary productivity (NPP) across the simulation years.
#'
#' @param cohortGroupKeep TODO
#' @param NPP TODO
#' @template masterRaster
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table copy setkey
#' @importFrom ggforce theme_no_axes
#' @importFrom ggplot2 ggplot geom_raster aes scale_fill_continuous ggtitle coord_fixed
#' @importFrom terra rast res unwrap values
NPPplot <- function(cohortGroupKeep, NPP, masterRaster) {
  masterRaster <- terra::unwrap(masterRaster)
  # Get a data.table with the pixel index of each cohort group at each time step
  cohortGroupKeep <- melt.data.table(
    cohortGroupKeep,
    id.vars = "pixelIndex",
    measure.vars = as.character(unique(NPP$simYear)),
    variable.name = "simYear",
    value.name = "cohortGroupID",
    na.rm = TRUE,
    variable.factor = FALSE
  )
  cohortGroupKeep[, simYear := as.integer(simYear)]
  # Match npp to pixel index with the combination of simYear and cohortGroup
  # allow.cartesian: multiple cohort group per pixel and vice-versa.
  npp <- merge(
    as.data.table(NPP),
    cohortGroupKeep,
    by = c("simYear", "cohortGroupID"),
    allow.cartesian = TRUE
  )
  # Calculate total NPP per pixel x year
  npp[, `:=`(totalNPP, sum(NPP)), by = c("simYear", "pixelIndex")]
  npp <- unique(npp[,.(pixelIndex, simYear, totalNPP)])
  # Calculate average NPP across year per pixel
  temp <- npp[, .(avgNPP = mean(totalNPP)), by = pixelIndex]
  setkey(temp, pixelIndex)
  # Create raster to plot
  plotMaster <- terra::rast(masterRaster)
  names(plotMaster) <- "avgNPP"
  plotMaster[] <- NA
  plotMaster[temp$pixelIndex] <- temp$avgNPP
  # Calculate the landscape-level avg NPP (for the plot title)
  pixSize <- prod(res(masterRaster))/10000
  temp[, `:=`(pixNPP, avgNPP * pixSize)]
  overallAvgNpp <- sum(temp$pixNPP)/(nrow(temp) * pixSize)
  # Convert to data.frame to use with geom_raster.
  plotMaster <- as.data.frame(plotMaster, xy = TRUE)
  Plot <- ggplot() + geom_raster(data = plotMaster, aes(x = x, y = y, fill = avgNPP)) +
    theme_no_axes() + scale_fill_continuous(low = "#873f38", high = "#61d464", na.value = "transparent", guide = "colorbar") + labs(fill = "NPP (MgC)" ) +
    ggtitle(paste0("Pixel-level average NPP\n",
                   "Landscape average: ", round(overallAvgNpp, 3), "  MgC/ha/yr.")) + coord_fixed()
}


#' `barPlot`
#'
#' @param cbmPools TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table as.data.table melt.data.table
#' @importFrom ggplot2 aes expansion geom_col ggplot ggtitle guides guide_legend labs
#' scale_fill_brewer scale_fill_discrete scale_y_continuous theme_classic
barPlot <- function(cbmPools) {
  cbmPools <- as.data.table(cbmPools)
  cbmPools$cohortGroupID <- as.character(cbmPools$cohortGroupID)
  pixelNo <- sum(cbmPools$N/length(unique(cbmPools$simYear)))
  cbmPools$simYear <- as.character(cbmPools$simYear)
  carbonCompartments <- cbmPools[, .(soil = sum(AboveGroundVeryFastSoil, BelowGroundVeryFastSoil,
                                                AboveGroundFastSoil, BelowGroundFastSoil,
                                                AboveGroundSlowSoil, BelowGroundSlowSoil, MediumSoil),
                                     AGlive = sum(Merch, Foliage, Other),
                                     BGlive = sum(CoarseRoots,FineRoots),
                                     snags = sum(StemSnag, BranchSnag), weight = N/pixelNo),
                                 by = .(cohortGroupID, simYear)]
  outTable <- carbonCompartments[, .(soil = sum(soil * weight),
                                     AGlive = sum(AGlive * weight),
                                     BGlive = sum(BGlive * weight),
                                     snags = sum(snags * weight)),
                                 by = simYear]
  outTable <- data.table::melt.data.table(outTable, id.vars = "simYear",
                                          measure.vars = c("soil", "AGlive", "BGlive", "snags"),
                                          variable.name = "pool", value.name = "carbon")
  outTable$simYear <- as.numeric(outTable$simYear)
  outTable$carbon <- as.numeric(outTable$carbon)
  barPlots <- ggplot(data = outTable, aes(x = simYear, y = carbon, fill = pool)) +
    geom_col(position = "fill") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_fill_discrete(name = "Carbon Compartment") +
    labs(x = "Year", y = "Proportion") + theme_classic() + ggtitle("Proportion of C above and below ground compartments.") +
    guides(fill = guide_legend(title.position= "top", title ="Carbon compartment") ) +
    scale_fill_brewer(palette = "Set1", labels = c("Soil", "AGlive", "BGlive", 'snags'))
}
