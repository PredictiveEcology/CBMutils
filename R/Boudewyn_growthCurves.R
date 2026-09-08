utils::globalVariables(c(
  "a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3",
  "Age", "eco", "gcids",
  "spatialUnitID", "SpatialUnitID", "species", "speciesName",
  "p_sw_low", "p_sw_high", "p_sb_low", "p_sb_high",
  "p_br_low", "p_br_high", "p_fl_low", "p_fl_high"
))

#' Calculate stemwood biomass (per ha) of live merchantable trees
#'
#' Implements equation 1 of Boudewyn et al. (2007) to determines the total stemwood biomass of
#' merchantable trees (in metric tonnes per hectare; \eqn{T/ha}),
#' using parameters \eqn{a} and \eqn{b} from Table 3.
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param vol gross merchantable volume per hectare (\eqn{m^3/ha})
#' @template bTable3
#'
#' @return stemwood biomass of merchantable trees (\eqn{b_m} in units \eqn{T/ha})
#'
#' @importFrom data.table as.data.table fread is.data.table
#' @export
b_m <- function(vol,
                bTable3 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv"){

  # Read Boudewyn parameters
  if (!is.data.table(bTable3)) bTable3 <- ifelse(is.data.frame(bTable3), as.data.table(bTable3), fread(bTable3))

  # flag if vol in growth curve is above the max vol the model was developed on
  if (!is.na(unique(bTable3$volm))) {
    if (max(vol) > unique(bTable3$volm)) {
      message("The volumes in the growth information provided are greater than the maximum volume ",
              "the stem wood model was developed with.")
    }
  }
  b_m <- unique(bTable3$a) * vol ^ unique(bTable3$b)
  return(b_m)
}

#' Expansion factor for non-merchantable live tree biomass
#'
#' Implements equation 2 of Boudewyn et al. (2007), used to determine the total stem wood biomass
#' (in metric tonnes per hectare; \eqn{T/ha}) of non-merchantable trees (\eqn{B_n}), together
#' with the stemwood biomass of live merchantable and non-merchantable trees (\eqn{B_{nm}}),
#' using parameters \eqn{a}, \eqn{b}, and \eqn{k} from Table 4.
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param eq1 stemwood biomass of merchantable trees (\eqn{T/ha}) from equation 1 of
#' Boudewyn et al. (2007) (i.e., the result of \code{\link{b_m}}).
#' @param vol gross merchantable volume per hectare (\eqn{m^3/ha})
#' @template bTable4
#'
#' @return two-column matrix with columns corresponding to \eqn{b_n} and \eqn{b_{nm}}
#'
#' @importFrom data.table as.data.table fread is.data.table
#' @export
nmfac <- function(eq1, vol,
                  bTable4 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv") {

  # Read Boudewyn parameters
  if (!is.data.table(bTable4)) bTable4 <- ifelse(is.data.frame(bTable4), as.data.table(bTable4), fread(bTable4))

  # flag if vol in growth curve is above the max vol the model was developed on
  if (!is.na(unique(bTable4$volm))) {
    if (max(vol) > unique(bTable4$volm)) {
      message("The volumes in the growth information provided are greater than the maximum volume ",
              "the non-merch sized tree model was developed with.")
    }
  }
  nmFac <- unique(bTable4$k) + (unique(bTable4$a) * eq1 ^ unique(bTable4$b))
  # caps on non-merch trees provided in table 4
  nmFac[which(nmFac > bTable4$cap)] <- unique(bTable4$cap)
  b_nm <- nmFac * eq1
  b_n <- b_nm - eq1
  return(cbind(b_n = b_n, b_nm = b_nm))
}

#' Expansion factor for sapling-sized trees
#'
#' Implements equation 3 of Boudewyn et al. (2007), used to determine the total stem wood biomass
#' (in metric tonnes per hectare; \eqn{T/ha}) of sapling-sized trees (\eqn{B_s}),
#' using parameters \eqn{a}, \eqn{b}, and \eqn{k} from Table 5.
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param eq2 two-column matrix giving stemwood biomass of non-merchantable trees
#' (i.e., \eqn{b_n} given in units \eqn{T/ha}), and merchantable + non-merchantable trees
#' (i.e., \eqn{b_{nm}} given in units \eqn{T/ha}), from equation 2 of Boudewyn et al. (2007)
#' (i.e., the result of \code{\link{nmfac}}).
#' @param vol gross merchantable volume per hectare (\eqn{m^3/ha})
#' @template bTable5
#'
#' @return stemwood biomass of sapling-sized trees (\eqn{b_s} in units \eqn{T/ha})
#'
#' @importFrom data.table as.data.table fread is.data.table
#' @export
sapfac <- function(eq2, vol,
                   bTable5 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv"){

  # Read Boudewyn parameters
  if (!is.data.table(bTable5)) bTable5 <- ifelse(is.data.frame(bTable5), as.data.table(bTable5), fread(bTable5))

  # flag if vol in growth curve is above the max vol the model was developed on
  if (!is.na(unique(bTable5$volm))) {
    if (max(vol) > unique(bTable5$volm)) {
      message("The volumes in the growth information provided are greater than the maximum volume ",
              "the sappling model was developed with.")
    }
  }
  # caps on sapling fraction provided in bTable5
  sapFac <- unique(bTable5$k) + (unique(bTable5$a) * eq2[, 2] ^ unique(bTable5$b))
  sapFac[which(sapFac > bTable5$cap)] <- unique(bTable5$cap)
  b_snm <- sapFac * eq2[, 2]
  b_s <- b_snm - eq2[, 2]
  return(b_s)
}

#' Proportions of total tree biomass in stemwood, bark, branches, and foliage
#'
#' Implements equations 4-7 of Boudewyn et al. (2007), used to determine the proportions
#' of total tree biomass in stemwood, bark, branches, and foliage
#' (\eqn{p_{stemwood}}, \eqn{p_{bark}}, \eqn{p_{branches}}, \eqn{p_{foliage}}, respectively),
#' using parameters \eqn{a}, \eqn{b} from Table 6 and volume-proportion caps
#' from Table 7.
#'
#' TODO: will eventually add species, ecozone
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param x `vector` gross merchantable volume per hectare (\eqn{m^3/ha}) or
#' total biomass (\eqn{tonnes/ha})
#' @param type `character` specifies if the `x` represents gross merchantable
#' volume per hectare ("volume") or total biomass ("biomass").
#' @template bTable6
#' @template bTable6tb
#' @template bTable7
#' @template bTable7tb
#'
#' @return four-column matrix will columns corresponding to \eqn{p_{stemwood}}, \eqn{p_{bark}},
#' \eqn{p_{branches}}, and \eqn{p_{foliage}}
#'
#' @importFrom data.table as.data.table fread is.data.table
#' @export
biomProp <- function(x, type = "volume",
                     bTable6   = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv",
                     bTable7   = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv",
                     bTable6tb = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv",
                     bTable7tb = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv"){

  # Read Boudewyn parameters
  if (!type %in% c("volume", "biomass")) stop("The argument type in biomProp() needs to be `volume` or `biomass`")
  if (type == "biomass"){
    bTable6 <- bTable6tb
    bTable7 <- bTable7tb
  }
  if (!is.data.table(bTable6)) bTable6 <- ifelse(is.data.frame(bTable6), as.data.table(bTable6), fread(bTable6))
  if (!is.data.table(bTable7)) bTable7 <- ifelse(is.data.frame(bTable7), as.data.table(bTable7), fread(bTable7))

  if (type == "volume"){
    if(any(!(c("vol_min", "vol_max") %in% colnames(bTable7)))) {
      stop("The parameter tables do not have the correct columns for ", type, " inputs.")
    }
    caps <- as.numeric(bTable7[1 ,c("vol_min", "vol_max")])
  }
  if (type == "biomass") {
    if(any(!(c("biom_min", "biom_max") %in% colnames(bTable7)))) {
      stop("The parameter tables do not have the correct columns for ", type, " inputs.")
    }
    caps <- as.numeric(bTable7[1 ,c("biom_min", "biom_max")])
  }

  # flag if vol in below vol_min or above vol_max (when not NA)
  # the model was developed on
  # DC 2025-03-07: ONLY FOR VOLUME. MUTED FOR BIOMASS BECAUSE IT HAPPENS ALL THE
  # TIME WHEN CREATING YIELD TABLES FROM LANDR
  if (length(is.na(unique(caps[1]))) > 0 & type == "volume") {
    testVec <- min(x) < unique(caps[1])
    if (any(testVec)) {
      message("Some volumes in the growth information provided are smaller than ",
              "the minimum volume the proportions model was developed with.")
    }
  }

  if (length(is.na(unique(caps[2]))) > 0 & type == "volume") {
    testVec <- max(x) > unique(caps[2])
    if (any(testVec)) {
      message("Some volumes in the growth information provided are larger than ",
              "the maximum volume the proportions model was developed with.")
    }
  }


  lvol <- log(x + 5)

 ## denominator is the same for all 4 equations
  denom <- (1 + exp(bTable6[1, a1] + bTable6[1, a2] * x + bTable6[1, a3] * lvol) +
              exp(bTable6[1, b1] + bTable6[1, b2] * x + bTable6[1, b3] * lvol) +
              exp(bTable6[1, c1] + bTable6[1, c2] * x + bTable6[1, c3] * lvol))
  ## for each proportion, enforce caps per table 7
  pstem <- 1 / denom
  pstem[which(x < caps[1])] <- bTable7[1, p_sw_low]
  pstem[which(x > caps[2])] <- bTable7[1, p_sw_high]

  pbark <- exp(bTable6[1, a1] + bTable6[1, a2] * x + bTable6[1, a3] * lvol) / denom
  pbark[which(x < caps[1])] <- bTable7[1, p_sb_low]
  pbark[which(x > caps[2])] <- bTable7[1, p_sb_high]

  pbranches <- exp(bTable6[1, b1] + bTable6[1, b2] * x + bTable6[1, b3] * lvol) / denom
  pbranches[which(x < caps[1])] <- bTable7[1, p_br_low]
  pbranches[which(x > caps[2])] <- bTable7[1, p_br_high]

  pfol <- exp(bTable6[, c1] + bTable6[1, c2] * x + bTable6[1, c3] * lvol) / denom
  pfol[which(x < caps[1])] <- bTable7[1, p_fl_low]
  pfol[which(x > caps[2])] <- bTable7[1, p_fl_high]

  propVect <- cbind(pstem = pstem, pbark = pbark, pbranches = pbranches, pfol = pfol)

  if(any(abs(rowSums(propVect) - 1) > 0.001)) {
    stop("The sums of biomass proportions do not sum to 1...")
  }

  return(propVect)
}

#' Calculate biomass from gross merchantable volume
#'
#' Implements the flowchart from figure 3 of Boudewyn et al. (2007) to determined the
#' total above ground biomass (\eqn{T/ha}) from gross merchantable volume (\eqn{m^3/ha}).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param meta Growth curve metadata
#' @param gCvalues Growth curve volume values for each age
#' @param spsMatch Species associated with growth curve
#' @param ecozones Ecozone associated with the growth curve
#' @template bTable3
#' @template bTable4
#' @template bTable5
#' @template bTable6
#' @template bTable7
#'
#' @return three-column matrix with columns corresponding to biomass (\eqn{T/ha}) for
#' total merchantable, foliage, and other.
#'
#' @importFrom data.table as.data.table fread is.data.table
#' @export
convertM3biom <- function(meta, gCvalues, spsMatch, ecozones,
                          bTable3 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv",
                          bTable4 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv",
                          bTable5 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv",
                          bTable6 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv",
                          bTable7 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv") {

  # Read Boudewyn parameters
  if (!is.data.table(bTable3)) bTable3 <- ifelse(is.data.frame(bTable3), as.data.table(bTable3), fread(bTable3))
  if (!is.data.table(bTable4)) bTable4 <- ifelse(is.data.frame(bTable4), as.data.table(bTable4), fread(bTable4))
  if (!is.data.table(bTable5)) bTable5 <- ifelse(is.data.frame(bTable5), as.data.table(bTable5), fread(bTable5))
  if (!is.data.table(bTable6)) bTable6 <- ifelse(is.data.frame(bTable6), as.data.table(bTable6), fread(bTable6))
  if (!is.data.table(bTable7)) bTable7 <- ifelse(is.data.frame(bTable7), as.data.table(bTable7), fread(bTable7))

  oneCurve <- gCvalues[gcids == meta$gcids, ]
  # the Boudewyn models do not deal with 0s
  oneCurve <- oneCurve[Age != 0,]
  spec <- unique(spsMatch[species == meta$species, ]$canfi_species)
  ## might have to put in a loop here for each ecozone?
  ez <- ecozones[SpatialUnitID == meta$spatial_unit_id, ]$EcoBoundaryID
  jurisID <- ecozones[SpatialUnitID == meta$spatial_unit_id, ]$abreviation
  gen <- unique(spsMatch[species == meta$species, ]$genus)

  bTable3 <- bTable3[canfi_species == spec & ecozone == ez & juris_id == jurisID,]
  bTable4 <- bTable4[canfi_species == spec & ecozone == ez & juris_id == jurisID,]
  bTable6 <- bTable6[canfi_species == spec & ecozone == ez & juris_id == jurisID,]
  bTable7 <- bTable7[canfi_species == spec & ecozone == ez & juris_id == jurisID,]
  # table 5 is different than the others
  if (any(!jurisID %in% bTable5$juris_id)){
    abreviation <- c("PE", "QC", "ON", "MB", "SK", "YK", "NU", "NS")
    tabreviation <- c("NB", "NL", "NL", "AB", "AB", "NT", "NT", "NB")
    abreviationReplace <- data.table(abreviation, tabreviation)
    thisAdminT <- merge(abreviationReplace, ecozones)
    thisAdminT[, c("abreviation", "tabreviation") := list(tabreviation, NULL)]
    jurisID <- thisAdminT[SpatialUnitID == meta$spatial_unit_id, ]$abreviation
  }
  bTable5 <- bTable5[genus == gen & ecozone == ez & juris_id == jurisID,]

  # Equations are numbered following the flowchart of the biomass model application in
  # Boudewyn et al. 2007 p7 (Fig3)
  # eq1 returns the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in bTable3
  eq1 <- b_m(oneCurve$MerchVolume, bTable3 = bTable3)
  # eq2 returns a two column matrix giving the biomass of the non-merch sized
  # trees (b_n) and b_nm which is the sum of the total stem wood biomass of merch size
  # live plus, the stem wood live of non merch-sized trees, given the total
  # stem wood biomass per ha of live merch size trees (in tonnes/ha)
  eq2 <- nmfac(eq1 = eq1, vol = oneCurve$MerchVolume, bTable4 = bTable4)
  # eq3 is for biomass of the saplings, the smallest of the non-merch trees. The
  # non-merch biomass from eq2, is needed. eq3 returns b_s, stem wood biomass of
  # live sapling-sized trees in tonnes/ha
  eq3 <- sapfac(eq2 = eq2, vol = oneCurve$MerchVolume, bTable5 = bTable5)
  #eq3[which(is.na(eq3))] <- 0
  # middle box flowchart3: total stem wood biomass (tonnes) /ha for all live trees

  totalStemWood <- eq1 + eq2[,1] + eq3
  totalStemWood[which(is.nan(totalStemWood))] <- NA
  # calculate the 4 proportions that should be returned: proportion for
  # stemwood, prop for bark, prop for branches, and prop for foliage.
  pVect <- biomProp(bTable6 = bTable6, bTable7 = bTable7, x = oneCurve$MerchVolume)
  # translating this into biomass values for the carbon pools
  totMerch <- eq1
  totTree <- totalStemWood / pVect[, 1]
  bark <- totTree * pVect[, 2]
  branch <- totTree * pVect[, 3]
  fol <- totTree * pVect[, 4]
  other <- branch + bark + eq2[, 1] + eq3
  biomCumulative <- as.matrix(cbind(totMerch = totMerch, fol = fol, other = other))
  return(biomCumulative)
}
