utils::globalVariables(c(
  "a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3",
  "Age", "canfi_species", "eco", "ecozone", "genus", "GrowthCurveComponentID",
  "spatialUnitID", "species", "speciesName"
))

#' b_m
#'
#' eq1 gives the total stem wood biomass in metric tonnes/ha, when you give it
#' the gross merchantable volume/ha. Parameters a and b are in table3.
#'
#' @param table3 DESCRIPTION NEEDED
#' @param vol DESCRIPTION NEEDED
#'
#' @export
b_m <- function(table3, vol) {
  # flag is vol in growth curve is above the max vol the model was developed on
  if (!is.na(unique(table3$volm))) {
    if (max(vol) > unique(table3$volm)) {
      message("The volumes in the growth information provided are greater than the maximum volume ",
              "the stem wood model was developed with.")
    }
  }
  b_m <- unique(table3$a) * vol ^ unique(table3$b)
  return(b_m)
}

#' nmfac
#'
#' eq2 is for non-merch sized trees.
#'
#' @param table4 DESCRIPTION NEEDED
#' @param eq1 DESCRIPTION NEEDED
#' @param vol DESCRIPTION NEEDED
#'
#' @export
nmfac <- function(table4, eq1, vol) {
  # flag is vol in growth curve is above the max vol the model was developed on
  if (!is.na(unique(table4$volm))) {
    if (max(vol) > unique(table4$volm)) {
      message("The volumes in the growth information provided are greater than the maximum volume ",
              "the non-merch sized tree model was developed with.")
    }
  }
  nmFac <- unique(table4$k) + (unique(table4$a) * eq1 ^ unique(table4$b))
  # caps on non-merch trees provided in table 4
  nmFac[which(nmFac > table4$cap)] <- unique(table4$cap)
  b_nm <- nmFac * eq1
  b_n <- b_nm - eq1
  return(cbind(b_n, b_nm))
}

#' sapfac
#'
#' `eq3` is for the saplings and it needs `b_nm` from the previous `eq2`
#'
#' @param table5 DESCRIPTION NEEDED
#' @param eq2 DESCRIPTION NEEDED
#' @param vol DESCRIPTION NEEDED
#'
#' @export
sapfac <- function(table5, eq2, vol){
  # flag is vol in growth curve is above the max vol the model was developed on
  if (!is.na(unique(table5$volm))) {
    if (max(vol) > unique(table5$volm)) {
      message("The volumes in the growth information provided are greater than the maximum volume ",
              "the sappling model was developed with.")
    }
  }
  # caps on sapling fraction provided in table5
  sapFac <- unique(table5$k) + (unique(table5$a) * eq2[,2] ^ unique(table5$b))
  sapFac[which(sapFac > table5$cap)] <- unique(table5$cap)
  b_snm <- sapFac * eq2[,2]
  b_s <- b_snm - eq2[, 2]
  return(b_s)
}

#' calculate the 4 proportions that should be returned
#'
#' will eventually add species, ecozone
#' vol = gross merchantable volume per ha
#' lvol = natural logarithm of (vol+5)
#'
#' @param table6 DESCRIPTION NEEDED
#' @param table7 DESCRIPTION NEEDED
#' @param vol DESCRIPTION NEEDED
#'
#' @export
biomProp <- function(table6, table7, vol) {
  # flag if vol in below vol_min or above vol_max (when not NA)
  # the model was developed on
  if (!is.na(unique(table7$vol_min))) {
    if (min(vol) < unique(table7$vol_min)) {
      message("Some volumes in the growth information provided are smaller than the minumum volume ",
              "the proportions model was developed with.")
    }
  }

  if (!is.na(unique(table7$vol_max))) {
    if (max(vol) > unique(table7$vol_max)) {
      message("Some volumes in the growth information provided are larger than the maximumum ",
              "volume the proportions model was developed with.")
    }
  }

  lvol <- log(vol + 5)
  pstem <- 1 / (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
                  exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
                  exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  # caps
  pstem[which(vol < unique(table7$vol_min))] <- unique(table7$p_sw_low)
  pstem[which(vol > unique(table7$vol_max))] <- unique(table7$p_sw_high)

  pbark <- exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) /
    (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
       exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
       exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  pbark[which(vol < unique(table7$vol_min))] <- unique(table7$p_sb_low)
  pbark[which(vol > unique(table7$vol_max))] <- unique(table7$p_sb_high)

  pbranches <- exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) /
    (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
       exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
       exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  pbranches[which(unique(vol < table7$vol_min))] <- unique(table7$p_br_low)
  pbranches[which(unique(vol > table7$vol_max))] <- unique(table7$p_br_high)

  pfol <- exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol) /
    (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
       exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
       exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  pfol[which(vol < unique(table7$vol_min))] <- unique(table7$p_fl_low)
  pfol[which(vol > unique(table7$vol_max))] <- unique(table7$p_fl_high)

  propVect <- cbind(pstem,pbark,pbranches,pfol)
  return(propVect)
}

#' convertM3biom
#'
#' DESCRIPTION NEEDED
#'
#' @param meta DESCRIPTION NEEDED
#' @param gCvalues DESCRIPTION NEEDED
#' @param spsMatch DESCRIPTION NEEDED
#' @param ecozones DESCRIPTION NEEDED
#' @param params3 DESCRIPTION NEEDED
#' @param params4 DESCRIPTION NEEDED
#' @param params5 DESCRIPTION NEEDED
#' @param params6 DESCRIPTION NEEDED
#' @param params7 DESCRIPTION NEEDED
#'
#' @export
convertM3biom <- function(meta, gCvalues, spsMatch, ecozones, params3, params4, params5, params6,
                          params7) {
  oneCurve <- gCvalues[GrowthCurveComponentID == meta$growth_curve_component_id, ]
  # the Boudewyn models do not deal with 0s
  oneCurve <- oneCurve[Age != 0,]
  spec <- unique(spsMatch[species == meta$species,]$canfi_species)
  ## might have to put in a loop here for each ecozone?
  ez <- ecozones[SpatialUnitID == meta$spatial_unit_id,]$EcoBoundaryID
  gen <- unique(spsMatch[species == meta$species,]$genus)

  params3 <- params3[canfi_species == spec & ecozone == ez,]
  params4 <- params4[canfi_species == spec & ecozone == ez,]
  # table 5 is different than the others
  params5 <- params5[genus == gen & ecozone == ez,]
  params6 <- params6[canfi_species == spec & ecozone == ez,]
  params7 <- params7[canfi_species == spec & ecozone == ez,]
  # Equations are numbered following the flowchart of the biomass model application in
  # Boudewyn et al 2007 p7 (Fig3)
  # eq1 returns the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in table3
  eq1 <- b_m(params3, oneCurve$MerchVolume)
  # eq2 returns a two column matrix giving the biomass of the non-merch sized
  # trees (b_n) and b_nm which is the sum of the total stem wood biomass of merch size
  # live plus, the stem wood live of non merch-sized trees, given the total
  # stem wood biomass per ha of live merch size trees (in tonnes/ha)
  eq2 <- nmfac(params4, eq1 = eq1, vol = oneCurve$MerchVolume)
  # eq3 is for biomass of the saplings, the smallest of the non-merch trees. The
  # non-merch biomass from eq2, is needed. eq3 returns b_s, stem wood biomass of
  # live sapling-sized trees in tonnes/ha
  eq3 <- sapfac(params5, eq2 = eq2, vol = oneCurve$MerchVolume)
  #eq3[which(is.na(eq3))] <- 0
  # middle box flowchart3: total stem wood biomass (tonnes) /ha for all live trees

  merch <- eq1 + eq2[,1] + eq3
  merch[which(is.nan(merch))] <- NA
  # calculate the 4 proportions that should be returned: proportion for
  # stemwood, prop for bark, prop for branches, and prop for foliage.
  pVect <- biomProp(table6 = params6, table7 = params7, vol = oneCurve$MerchVolume)
  # translating this into biomass values for the carbon pools
  totMerch <- merch / pVect[, 1]
  bark <- totMerch * pVect[, 2]
  branch <- totMerch * pVect[, 3]
  fol <- totMerch * pVect[,4]
  other <- branch + bark
  biomCumulative <- as.matrix(cbind(totMerch,fol,other))
  return(biomCumulative)
}
