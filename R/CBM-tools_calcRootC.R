
utils::globalVariables(c(
  "sw", "AG", "SoftwoodAG", "HardwoodAG", "SoftwoodAGB", "HardwoodAGB",
  "SoftwoodRootB", "HardwoodRootB", "SoftwoodRootProp", "HardwoodRootProp"
))

#' `calcRootC`
#'
#' `calcRootC` calculates the mass of carbon in roots pools from above ground pools
#'
#' @param aboveGroundC data.table of above ground biomass with the mass of carbon (tonnes/ha) in each pool.
#' Columns can either be `sw` (Softwood = TRUE, hardwood = FALSE), `Merch`, `Foliage` and `Other`,
#' or `SoftwoodMerch`, `HardwoodMerch`, `SoftwoodFoliage`, `HardwoodFoliage`, `SoftwoodOther`, and `HardwoodOther`.
#' @param a_sw "a" value for softwood root biomass
#' @param b_sw "b" value for softwood root biomass
#' @param a_hw "a" value for hardwood root biomass
#' @param b_hw "b" value for hardwood root biomass
#' @param a_frp "a" value for fine root proportion
#' @param b_frp "b" value for fine root proportion
#' @param c_frp "c" value for fine root proportion
#' @param biomassToCarbonRate Conversion factor of biomass to carbon
#'
#' @references
#' Li, Z., Kurz, W. A., Apps, M. J., & Beukema, S. J. (2003). Belowground biomass
#' dynamics in the Carbon Budget Model of the Canadian Forest Sector: recent improvements
#' and implications for the estimation of NPP and NEP. Canadian journal of forest
#' research, 33(1), 126-136.
#'
#' @returns data.table with mass of carbon (tonnes/ha) in coarseRoots and fineRoots pools.
#' @export
#'
calcRootC <- function(aboveGroundC,
                      a_sw = 0.222, b_sw = 1,
                      a_hw = 1.576, b_hw = 0.615,
                      a_frp = 0.072, b_frp = 0.354, c_frp = -0.060212,
                      biomassToCarbonRate = 0.5){

  # Choose column set
  AGcols <- list(
    c("SoftwoodMerch", "HardwoodMerch", "SoftwoodFoliage", "HardwoodFoliage", "SoftwoodOther", "HardwoodOther"),
    c("sw", "Merch", "Foliage", "Other")
  )

  whichCols <- which(sapply(AGcols, function(cols) all(tolower(cols) %in% tolower(names(aboveGroundC)))))
  if (length(whichCols) == 0) stop(
    "aboveGroundC needs one of these column sets:\n- ",
    paste(sapply(AGcols, function(AGcol) paste(shQuote(AGcol), collapse = ", ")),
          collapse = "\n- "))

  AGcols <- AGcols[[whichCols[[1]]]]
  aboveGroundC <- data.table::as.data.table(aboveGroundC)
  data.table::setnames(aboveGroundC, tolower(AGcols), AGcols, skip_absent = TRUE)
  aboveGroundC <- aboveGroundC[, .SD, .SDcols = AGcols]

  # Calculate the total above ground mass of carbon
  if (whichCols[[1]] == 1){

    aboveGroundC[, SoftwoodAG := rowSums(aboveGroundC[, .(SoftwoodMerch, SoftwoodFoliage, SoftwoodOther)])]
    aboveGroundC[, HardwoodAG := rowSums(aboveGroundC[, .(HardwoodMerch, HardwoodFoliage, HardwoodOther)])]

  }else{

    if (!is.logical(aboveGroundC$sw)) stop("aboveGroundC 'sw' column must be logical")

    aboveGroundC[, AG := rowSums(aboveGroundC[, .(Merch, Foliage, Other)])]
    aboveGroundC[, SoftwoodAG := data.table::fifelse( sw, AG, 0)]
    aboveGroundC[, HardwoodAG := data.table::fifelse(!sw, AG, 0)]
    aboveGroundC[, AG := NULL]
  }

  # Convert Mg/ha of Carbon to Mg/ha of biomass
  aboveGroundC[, SoftwoodAGB := SoftwoodAG / biomassToCarbonRate]
  aboveGroundC[, HardwoodAGB := HardwoodAG / biomassToCarbonRate]

  # Calculate root total biomass
  aboveGroundC[, SoftwoodRootB := a_sw * SoftwoodAGB^b_sw]
  aboveGroundC[, HardwoodRootB := a_hw * HardwoodAGB^b_hw]

  # Calculate the proportion of fine roots
  aboveGroundC[, SoftwoodRootProp := a_frp + b_frp * exp(c_frp * SoftwoodRootB)]
  aboveGroundC[, HardwoodRootProp := a_frp + b_frp * exp(c_frp * HardwoodRootB)]

  # Calculate tonnes/ha of carbon
  aboveGroundC[, SoftwoodCoarseRoots := biomassToCarbonRate * SoftwoodRootB * (1 - SoftwoodRootProp)]
  aboveGroundC[, SoftwoodFineRoots   := biomassToCarbonRate * SoftwoodRootB * SoftwoodRootProp]
  aboveGroundC[, HardwoodCoarseRoots := biomassToCarbonRate * HardwoodRootB * (1 - HardwoodRootProp)]
  aboveGroundC[, HardwoodFineRoots   := biomassToCarbonRate * HardwoodRootB * HardwoodRootProp]

  return(aboveGroundC[, .(SoftwoodCoarseRoots, HardwoodCoarseRoots, SoftwoodFineRoots, HardwoodFineRoots)])
}



