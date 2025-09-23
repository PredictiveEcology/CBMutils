utils::globalVariables(
  c(".I", "a1", "a2", "a3", "age", "b1", "b2", "b3", "B", "biom_max", "biom_min",
    "c1", "c2", "c3", "a", "b", "k", "cap", "minAge", "canfi_spec", "CanfiCode",
    "canfi_species", "curve_id", "ecozone", "foliage", "i.a1", "i.a2", "i.a3",
    "i.b1", "i.b2", "i.b3", "i.biom_max", "i.biom_min", "i.p_br_high", "i.p_br_low",
    "i.c1", "i.c2", "i.c3", "i.p_fl_high", "i.p_fl_low", "i.p_sb_high", "i.p_sb_low",
    "i.p_sw_high", "i.p_sw_low", "i.a", "i.b", "i.k", "i.cap", "i.minAge",
    "juris_id", "LandR", "merch", "other", "p_br_high", "p_br_low", "p_fl_high",
    "p_fl_low", "p_sb_high", "p_sb_low", "p_sw_high", "p_sw_low", "speciesCode"
  )
)

#' Convert total above ground biomass into 3 pools (\eqn{T/ha})
#'
#' Implements the flowchart from figure 3 of Boudewyn et al. (2007) using an alternative
#' set of parameter to divide total above ground biomass (\eqn{T/ha}) into total merchantable
#' stemwood biomass (\eqn{T/ha}), foliage biomass (\eqn{T/ha}), and other wood biomass (\eqn{T/ha}).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param allInfoAGBin `data.frame` with at least six following columns: `canfi_species`,
#' `speciesCode`, `ecozone`, `juris_id`, `age`, `B` and a column for pixel group identifier.
#'
#' @param table6 `data.frame` corresponding to Table 3 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#'
#' @param table7 `data.frame` corresponding to Table 4 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.
#'
#' @param tableMerchantability `data.frame` Parameters to estimate the proportion
#' of stemwood that is merchantable, approximates the relationship between stemwood
#' biomass and nonmerchfactor predicted by equation 2 of Boudewyn et al., 2007.
#'
#' @param pixGroupCol the name of the column in `allInfoAGBin` serving as the pixel group
#' identifier.

#' @return biomass (\eqn{T/ha}) in each above ground pool for each cohort per pixel group.
#'
#' @export
#' @importFrom data.table rbindlist setnames
cumPoolsCreateAGB <- function(allInfoAGBin, table6, table7, tableMerchantability, pixGroupCol){

  # 1. Input validation
  expectedColumns <- c("canfi_species", "juris_id", "ecozone", "age", "B", "speciesCode", pixGroupCol)
  if (any(!(expectedColumns %in% colnames(allInfoAGBin)))) {
    stop("The AGB table needs the following columns ", paste(expectedColumns, collapse = " "))
  }
  AGB <- as.data.table(allInfoAGBin, key = NULL)

  # 2. Get parameters for all curves
  # Identify all unique species/location combinations
  curves <- unique(AGB[, .(canfi_species, juris_id, ecozone)])

  # Get the parameters for each curve
  allParams <- getParameters(table6, table7, tableMerchantability, curves)

  # 3. Split biomass into pools

  ## IMPORTANT BOURDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ##
  AGB <- AGB[age > 0]

  # Call convertAGB2pools
  # It returns a data.table with merch, foliage, and other biomass pools
  biomassPools <- convertAGB2pools(AGB, allParams)

  # 5. Convert biomass to carbon mass
  biom2carbonConversionFactor <- 0.5
  biomassPools[, `:=`(
    merch = merch * biom2carbonConversionFactor,
    foliage = foliage * biom2carbonConversionFactor,
    other = other * biom2carbonConversionFactor
  )]


  # Combine identifier columns with the new carbon pools
  finalPools <- cbind(
    AGB[, .SD, .SDcols = c("speciesCode", "age", pixGroupCol)],
    biomassPools
  )

  return(finalPools)
}

#' Convert total above ground biomass into 3 pools (\eqn{T/ha})
#'
#' Implements the flowchart from figure 3 of Boudewyn et al. (2007) using an alternative
#' set of parameter to divide total above ground biomass (\eqn{T/ha}) into total merchantable
#' stemwood biomass (\eqn{T/ha}), foliage biomass (\eqn{T/ha}), and other wood biomass (\eqn{T/ha}).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param AGB `data.frame` with at least four following columns: `canfi_species`,
#' `ecozone`, `juris_id`, and `B`.
#'
#' @param allParams `data.frame` containing a row for each curve with all required
#' parameters of both `table6` and `table7` from Boudewyn et al. (2007) and parameters
#' on merchantability of stemwood.

#' @return three-column matrix with columns corresponding to biomass (\eqn{T/ha}) for
#' total merchantable, foliage, and other wood.
#'
#'
#' @export
convertAGB2pools <- function(AGB, allParams){
  AGBwithParams <- merge(AGB, allParams, by = c("canfi_species", "juris_id", "ecozone"), all.x = TRUE, sort = FALSE)

  # get the proportions of each pool
  pVect <- biomPropAGB(AGBwithParams)
  totTree <-  AGB$B
  totalStemWood <- totTree * pVect[, "pstem"]

  merch <- propMerch(totalStemWood, AGBwithParams) * totalStemWood

  # otherStemWood is everything that is not totMerch
  otherStemWood <- totalStemWood - merch
  bark <- totTree * pVect[,"pbark"]
  branch <- totTree * pVect[,"pbranches"]
  foliage <- totTree * pVect[,'pfol']
  other <- branch + bark + otherStemWood

  biomCumulative <- data.table(merch = merch, foliage = foliage, other = other)
  return(biomCumulative)
}

#' Extract the parameters to apply to convert total biomass into pool biomass
#'
#' Extract the species- and location- specific parameters for equation 4-7 of
#' Boudewyn et al. (2007). If there is no match for the given ecozone, the parameters
#' for a different ecozone in the same province/territory is returned. If there
#' is no match for a given province/territory, the parameters for a different
#' province/territory in the same ecozone is returned. If there is no match for
#' the given ecozone and province/territory, the parameters for a different location
#' is returned.
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param table6 `data.frame` corresponding to Table 3 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#'
#' @param table7 `data.frame` corresponding to Table 4 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.
#'
#' @param tableMerchantability `data.frame` Parameters to estimate the proportion
#' of stemwood that is merchantable, approximates the relationship between stemwood
#' biomass and nonmerchfactor predicted by equation 2 of Boudewyn et al., 2007.
#'
#' @param curves A `data.table` with unique combinations of `canfi_species`, `ecozone`, `juris_id`.
#'
#' @return A single `data.table` containing a row for each curve with all required
#'   parameters from both `table6` and `table7`.
getParameters <- function(table6, table7, tableMerchantability, curves){

  table6_dt <- as.data.table(table6)
  table7_dt <- as.data.table(table7)
  tableMerchantability_dt <- as.data.table(tableMerchantability)

  if (!all(
    curves$canfi_species %in% table6_dt$canfi_spec &
    curves$canfi_species %in% table7_dt$canfi_spec &
    curves$canfi_species %in% tableMerchantability_dt$canfi_species
  )) {
    missing_spp <- unique(curves$canfi_species[!(
      curves$canfi_species %in% table6_dt$canfi_spec &
        curves$canfi_species %in% table7_dt$canfi_spec &
        curves$canfi_species %in% tableMerchantability_dt$canfi_species
    )])
    stop("There are no parameters available for species: ",
         paste(missing_spp, collapse = ", "))
  }

  # Define parameter columns for clarity
  p6_cols <- c("a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3")
  p7_cols <- c("biom_min", "biom_max", "p_sw_low", "p_sb_low", "p_br_low", "p_fl_low",
               "p_sw_high", "p_sb_high", "p_br_high", "p_fl_high")
  pMerch_col <- c("a", "b", "k", "cap", "minAge")

  # Copy to avoid modifying the original 'curves' object
  params6 <- copy(curves)
  params7 <- copy(curves)
  paramsMerch <- copy(curves)

  # Merge parameters using a cascading join approach (from most to least specific)
  # Level 1: Exact match (species, ecozone, jurisdiction)
  params6[table6_dt,
          on = .(canfi_species = canfi_spec, ecozone, juris_id),
          (p6_cols) := mget(paste0("i.", p6_cols))]
  params7[table7_dt,
          on = .(canfi_species = canfi_spec, ecozone, juris_id),
          (p7_cols) := mget(paste0("i.", p7_cols))]
  paramsMerch[tableMerchantability_dt,
              on = .(canfi_species, ecozone, juris_id),
              (pMerch_col) := mget(paste0("i.", pMerch_col))]

  # Level 2: If no exact match, use parameters for species in same ecozone
  if(any(is.na(params6))){

    missingParameters <- which(is.na(params6[,"a1"]))
    params6[missingParameters] <- params6[missingParameters][table6_dt,
                                                             on = .(canfi_species = canfi_spec, ecozone),
                                                             (p6_cols) := mget(paste0("i.", p6_cols))]
    params7[missingParameters] <- params7[missingParameters][table7_dt,
                                                             on = .(canfi_species = canfi_spec, ecozone),
                                                             (p7_cols) := mget(paste0("i.", p7_cols))]
    paramsMerch[missingParameters] <- paramsMerch[missingParameters][tableMerchantability_dt,
                                                                     on = .(canfi_species, ecozone),
                                                                     (pMerch_col) := mget(paste0("i.", pMerch_col))]

    # Level 3: If still no match, use parameters for species in same jurisdiction
    if(any(is.na(params6))) {

      missingParameters <- which(is.na(params6[,"a1"]))
      params6[missingParameters] <- params6[missingParameters][table6_dt,
                                                               on = .(canfi_species = canfi_spec, juris_id),
                                                               (p6_cols) := mget(paste0("i.", p6_cols))]
      params7[missingParameters] <- params7[missingParameters][table7_dt,
                                                               on = .(canfi_species = canfi_spec, juris_id),
                                                               (p7_cols) := mget(paste0("i.", p7_cols))]
      paramsMerch[missingParameters] <- paramsMerch[missingParameters][tableMerchantability_dt,
                                                                       on = .(canfi_species, juris_id),
                                                                       (pMerch_col) := mget(paste0("i.", pMerch_col))]

      # Level 4: If still no match, use parameters for the same species wherever
      if(any(is.na(params6))) {
        missingParameters <- which(is.na(params6[,"a1"]))
        params6[missingParameters] <- params6[missingParameters][table6_dt,
                                                                 on = .(canfi_species = canfi_spec),
                                                                 (p6_cols) := mget(paste0("i.", p6_cols))]
        params7[missingParameters] <- params7[missingParameters][table7_dt,
                                                                 on = .(canfi_species = canfi_spec),
                                                                 (p7_cols) := mget(paste0("i.", p7_cols))]
        paramsMerch[missingParameters] <- paramsMerch[missingParameters][tableMerchantability_dt,
                                                                         on = .(canfi_species),
                                                                         (pMerch_col) := mget(paste0("i.", pMerch_col))]

      }
    }
  }

  allParams <- merge(params6, params7, by = c("canfi_species", "ecozone", "juris_id"), sort = FALSE)
  allParams <- merge(allParams, paramsMerch, by = c("canfi_species", "ecozone", "juris_id"), sort = FALSE)
  return(allParams)
}

#' Calculates proportions of total tree biomass in stemwood, bark, branches, and foliage
#'
#' Implements equations 4-7 of Boudewyn et al. (2007) adapted for biomass input
#' using parameters \eqn{a}, \eqn{b}, and \eqn{c }from Table 6 (`table6`) and
#' biomass-proportion caps from Table 7 (`table7`).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param AGBwithParams `data.frame` with both the total aboveground biomass `B`
#' to split and the parameters extracted from tables 6 and 7.
#'
#' @return four-column matrix will columns corresponding to \eqn{p_{stemwood}}, \eqn{p_{bark}},
#' \eqn{p_{branches}}, and \eqn{p_{foliage}}
#'
#' @export
biomPropAGB <- function(AGBwithParams) {

  propVect <- with(AGBwithParams, {

    lB <- log(B + 5)
    denom <- 1 + exp(a1 + a2 * B + a3 * lB) +
                exp(b1 + b2 * B + b3 * lB) +
                exp(c1 + c2 * B + c3 * lB)

    pstem <- 1 / denom
    pstem[which(B < biom_min)] <- p_sw_low[which(B < biom_min)]
    pstem[which(B > biom_max)] <- p_sw_high[which(B > biom_max)]

    pbark <- exp(a1 + a2 * B + a3 * lB) / denom
    pbark[which(B < biom_min)] <- p_sb_low[which(B < biom_min)]
    pbark[which(B > biom_max)] <- p_sb_high[which(B > biom_max)]

    pbranches <- exp(b1 + b2 * B + b3 * lB) / denom
    pbranches[which(B < biom_min)] <- p_br_low[which(B < biom_min)]
    pbranches[which(B > biom_max)] <- p_br_high[which(B > biom_max)]

    pfol <- exp(c1 + c2 * B + c3 * lB) / denom
    pfol[which(B < biom_min)] <- p_fl_low[which(B < biom_min)]
    pfol[which(B > biom_max)] <- p_fl_high[which(B > biom_max)]

    cbind(pstem = pstem, pbark = pbark, pbranches = pbranches, pfol = pfol)
  })

  if(any(abs(rowSums(propVect) - 1) > 0.001)) {
    stop("The sums of biomass proportions do not sum to 1...")
  }

  return(propVect)
}



#' Title
#'
#' @param totalStemWood
#' @param params
#'
#' @returns
#' @export
#'
propMerch <- function(totalStemWood, params){
  pMerch <- with(params,
                 ifelse(age > minAge,
                        k - exp(-a*(totalStemWood - b)),
                        0)
                 )
  return(pMerch)
}
