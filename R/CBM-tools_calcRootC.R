
utils::globalVariables(c(
  "..aboveGroundColumns"
))

#' `calcRootC`
#'
#' `calcRootC` calculates the mass of carbon in roots pools from above ground pools
#'
#' @param aboveGroundC data.table with mass of carbon (tonnes/ha) in the Merch, Foliage and Other pools
#' @param sw_hw a boolean vector indicating if the cohort is softwood (0) or hardwood (1)
#' @param a_sw "a" value for softwood root biomass
#' @param b_sw "b" value for softwood root biomass
#' @param a_hw "a" value for hardwood root biomass
#' @param b_hw "b" value for hardwood root biomass
#' @param a_frp "a" value for fine root proportion
#' @param b_frp "b" value for fine root proportion
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
calcRootC <- function(aboveGroundC, sw_hw,
                      a_sw = 0.222, b_sw = 1,
                      a_hw = 1.576, b_hw = 0.615,
                      a_frp = 0.072, b_frp = 0.354){

  aboveGroundColumns <- c("Merch", "Foliage", "Other")

  if(!all(aboveGroundColumns %in% names(aboveGroundC))) {
    stop("aboveGroundC needs the columns: ", paste(aboveGroundColumns, collapse = ", "))
  }
  aboveGroundC <- as.data.table(aboveGroundC)

  # Calculate the total above ground mass of carbon
  totAGC <- rowSums(aboveGroundC[, ..aboveGroundColumns])

  # Convert Mg/ha of Carbon to Mg/ha of biomass
  totAGB <- totAGC * 2

  # Calculate root total biomass
  if(!all(sw_hw %in% c(1,0))) {
    stop("sw_hw needs to be a boolean vector")
  }

  rootTotBiom <- ifelse(sw_hw == 0,
                        a_sw * totAGB^b_sw,
                        a_hw * totAGB^b_hw)

  # Calculate the proportion of fine roots
  fineRootProp <- a_frp + b_frp * exp(-0.060212 * rootTotBiom)


  # Calculate the proportion of fine roots
  rootBiom <- data.table(
    coarseRoots = rootTotBiom * (1 - fineRootProp),
    fineRoots = rootTotBiom * fineRootProp
  )

  # Reconvert into tonnes/ha of carbon
  rootC <- rootBiom * 0.5

  return(rootC)

}
