utils::globalVariables(c(
  "juris_id", "curve_id", ".I", "speciesCode", "canfi_spec", "CanfiCode", "LandR"
))

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
#' @param allInfoAGBin `data.frame` with at least four following columns: `canfi_species`,
#' `ecozone`, `juris_id`, `age`, `B` and a column for pixel group identifier.
#'
#' @param table6 `data.frame` corresponding to Table 3 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#'
#' @param table7 `data.frame` corresponding to Table 4 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.
#'
#' @param pixGroupCol the name of the column in `allInfoAGBin` serving as the pixel group
#' identifier.

#' @return biomass (\eqn{T/ha}) in each above ground pool for each cohort per pixel group.
#'
#' @export
#' @importFrom data.table rbindlist setnames
cumPoolsCreateAGB <- function(allInfoAGBin, table6, table7, pixGroupCol){

  # 1. Input validation
  expectedColumns <- c("canfi_species", "juris_id", "ecozone", "age", "B", pixGroupCol)
  if (any(!(expectedColumns %in% colnames(allInfoAGBin)))) {
    stop("The AGB table needs the following columns ", paste(expectedColumns, collapse = " "))
  }
  AGB <- as.data.table(allInfoAGBin, key = NULL)

  # 2. Get parameters for all curves
  # Identify all unique species/location combinations
  curves <- unique(AGB[, .(canfi_species, juris_id, ecozone)])

  # Get the parameters for each curve
  allParams <- getParameters(table6, table7, curves)

  # 3. Split biomass into pools

  ## IMPORTANT BOURDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ##
  AGB <- AGB[age > 0]

  # Call convertAGB2pool
  # It returns a data.table with merch, foliage, and other biomass pools
  biomassPools <- convertAGB2pools(AGB, allParams$params6, allParams$params7)

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
#' @param oneCurve `data.frame` with at least four following columns: `canfi_species`,
#' `ecozone`, `juris_id`, and `B`.
#'
#' @param table6 `data.frame` corresponding to Table 3 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#'
#' @param table7 `data.frame` corresponding to Table 4 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.

#' @return three-column matrix with columns corresponding to biomass (\eqn{T/ha}) for
#' total merchantable, foliage, and other wood.
#'
#'
#' @export
convertAGB2pools <- function(AGB, params6, params7){
  params6 <- merge(AGB, params6, all.x = TRUE)
  params7 <- merge(AGB, params7, all.x = TRUE)
  # get the proportions of each pool
  pVect <- biomPropAGB(table6 = params6, table7 = params7, x = AGB$B, type = "biomass")
  totTree <-  AGB$B
  totalStemWood <- totTree * pVect[, 1]

  ##TODO
  # find actual data on the proportion of totTree that is merch
  # Problem: CBM currently uses "merch" and "other" as C-pools. In these
  # equations (this function that matches the Boudewyn et al 2007 workflow),
  # totalStemwood is the sum of totMerch (eq1), b_n (eq2[,1] - stem wood biomass
  # of live, nonmerchantable-sized trees) and b_s (eq3 - stem wood biomass of
  # live, sapling-sized trees). The "merch" and the "other" C-pool requires us
  # to know the proportion of totalStemWood that is "merch" and "other"
  ##### IMPORTANT HARD CODING INFORMATION #######
  ## current fix: using the same parameters as FORCS (Forest Carbon Succession
  ## Extension V3.1). Eq 1 on p20 is PropStem = a *(1-b^Age) where a is 0.7546
  ## and b is 0.983. FORCS also sets a minimum merchantable age per species.
  ## Because we are in the RIA, I am setting that at 15. This needs to be a
  ## parameter either from LandR or set by the user (by provinces by species? -
  ## this is usually a diamter not an age)

  ### HARD CODED minimum merchantable age, a, b
  minMerchAge <-  15
  a <- 0.7546
  b <- 0.983

  # if age < MinMerchAge, the propMerch is 0, otherwise use FORCS, until we find actual data.
  propMerch <- (AGB$age >= minMerchAge) * a * (1-b^AGB$age)

  merch <- propMerch * totalStemWood

  # otherStemWood is everything that is not totMerch
  otherStemWood <- totalStemWood - merch

  bark <- totTree * pVect[, 2]
  branch <- totTree * pVect[, 3]
  foliage <- totTree * pVect[, 4]
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
#' @param canfi_species the canfi code of the species
#'
#' @param ecozone the code of the ecozone
#'
#' @param juris_id the 2-letter code for the province/territory

#' @return a list with 2 vectors for the parameters in table6 and table7 respectively.
getParameters <- function(table6, table7, curves){

  table6_dt <- as.data.table(table6)
  table7_dt <- as.data.table(table7)

  if (!all(curves$canfi_species %in% table6_dt$canfi_spec & curves$canfi_species %in% table7_dt$canfi_spec)) {
    missing_spp <- unique(curves$canfi_species[!(curves$canfi_species %in% table6_dt$canfi_spec &
                                                   curves$canfi_species %in% table7_dt$canfi_spec)])
    stop("There are no parameters available for species: ", paste(missing_spp, collapse = ", "))
  }

  # Define parameter columns for clarity
  p6_cols <- c("a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3")
  p7_cols <- c("biom_min", "biom_max", "p_sw_low", "p_sb_low", "p_br_low", "p_fl_low",
               "p_sw_high", "p_sb_high", "p_br_high", "p_fl_high")

  # Copy to avoid modifying the original 'curves' object
  params6 <- copy(curves)
  params7 <- copy(curves)

  # Merge parameters using a cascading join approach (from most to least specific)
  # Level 1: Exact match (species, ecozone, jurisdiction)
  params6[table6_dt,
          on = .(canfi_species = canfi_spec, ecozone, juris_id),
          (p6_cols) := mget(paste0("i.", p6_cols))]
  params7[table7_dt,
          on = .(canfi_species = canfi_spec, ecozone, juris_id),
          (p7_cols) := mget(paste0("i.", p7_cols))]

  # Level 2: If no exact match, use parameters for species in same ecozone
  if(any(is.na(params6))){

    missingParameters <- which(is.na(params6[,"a1"]))
    params6[missingParameters] <- params6[missingParameters][table6_dt,
                                                             on = .(canfi_species = canfi_spec, ecozone),
                                                             (p6_cols) := mget(paste0("i.", p6_cols))]
    params7[missingParameters] <- params7[missingParameters][table7_dt,
                                                             on = .(canfi_species = canfi_spec, ecozone),
                                                             (p7_cols) := mget(paste0("i.", p7_cols))]

    # Level 3: If still no match, use parameters for species in same jurisdiction
    if(any(is.na(params6))) {

      missingParameters <- which(is.na(params6[,"a1"]))
      params6[missingParameters] <- params6[missingParameters][table6_dt,
                                                               on = .(canfi_species = canfi_spec, juris_id),
                                                               (p6_cols) := mget(paste0("i.", p6_cols))]
      params7[missingParameters] <- params7[missingParameters][table7_dt,
                                                               on = .(canfi_species = canfi_spec, juris_id),
                                                               (p7_cols) := mget(paste0("i.", p7_cols))]

      # Level 4: If still no match, use parameters for the same species wherever
      if(any(is.na(params6))) {
        missingParameters <- which(is.na(params6[,"a1"]))
        params6[missingParameters] <- params6[missingParameters][table6_dt,
                                                                 on = .(canfi_species = canfi_spec),
                                                                 (p6_cols) := mget(paste0("i.", p6_cols))]
        params7[missingParameters] <- params7[missingParameters][table7_dt,
                                                                 on = .(canfi_species = canfi_spec),
                                                                 (p7_cols) := mget(paste0("i.", p7_cols))]
      }
    }
  }

  return(out = list(params6 = params6,
                    params7 = params7))
}

#' Proportions of total tree biomass in stemwood, bark, branches, and foliage
#'
#' Implements equations 4-7 of Boudewyn et al. (2007), used to determine the proportions
#' of total tree biomass in stemwood, bark, branches, and foliage
#' (\eqn{p_{stemwood}}, \eqn{p_{bark}}, \eqn{p_{branches}}, \eqn{p_{foliage}}, respectively),
#' using parameters \eqn{a}, \eqn{b} from Table 6 (`table6`) and volume-proportion caps
#' from Table 7 (`table7`).
#'
#' TODO: will eventually add species, ecozone
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param table6 `data.frame` corresponding to Table 6 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv>.
#' The alternative table 6 for equations using total biomass as independent variable
#' is available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#'
#' @param table7 `data.frame` corresponding to Table 7 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv>.
#' The alternative table 7 for equations using total biomass as independent variable
#' is available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.
#'
#' @param x `vector` gross merchantable volume per hectare (\eqn{m^3/ha}) or
#' total biomass (\eqn{tonnes/ha})
#'
#' @param type `character` specifies if the `x` represents gross merchantable
#' volume per hectare ("volume") or total biomass ("biomass").
#'
#' @return four-column matrix will columns corresponding to \eqn{p_{stemwood}}, \eqn{p_{bark}},
#' \eqn{p_{branches}}, and \eqn{p_{foliage}}
#'
#' @export
biomPropAGB <- function(table6, table7, x, type = "volume") {
  if (type == "volume"){
    if(any(!(c("vol_min", "vol_max") %in% colnames(table7)))) {
      stop("The parameter tables do not have the correct columns for ", type, " inputs.")
    }
    caps <- as.numeric(table7[ ,c("vol_min", "vol_max")])
  } else if (type == "biomass") {
    if(any(!(c("biom_min", "biom_max") %in% colnames(table7)))) {
      stop("The parameter tables do not have the correct columns for ", type, " inputs.")
    }
    caps <- table7[ ,c("biom_min", "biom_max")]
  } else {
    stop("The argument type in biomProp() needs to be `volume` or `biomass`")
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
  denom <- (1 + exp(table6[, a1] + table6[, a2] * x + table6[, a3] * lvol) +
              exp(table6[, b1] + table6[, b2] * x + table6[, b3] * lvol) +
              exp(table6[, c1] + table6[, c2] * x + table6[, c3] * lvol))
  ## for each proportion, enforce caps per table 7
  pstem <- 1 / denom
  pstem[which(x < caps[,1])] <- table7[which(x < caps[,1]), p_sw_low]
  pstem[which(x > caps[,2])] <- table7[which(x > caps[,2]), p_sw_high]

  pbark <- exp(table6[, a1] + table6[, a2] * x + table6[, a3] * lvol) / denom
  pbark[which(x < caps[,1])] <- table7[which(x < caps[,1]), p_sb_low]
  pbark[which(x > caps[,2])] <- table7[which(x > caps[,2]), p_sb_high]

  pbranches <- exp(table6[, b1] + table6[, b2] * x + table6[, b3] * lvol) / denom
  pbranches[which(x < caps[,1])] <- table7[which(x < caps[,1]), p_br_low]
  pbranches[which(x > caps[,2])] <- table7[which(x > caps[,2]), p_br_high]

  pfol <- exp(table6[, c1] + table6[, c2] * x + table6[, c3] * lvol) / denom
  pfol[which(x < caps[,1])] <- table7[which(x < caps[,1]), p_fl_low]
  pfol[which(x > caps[,2])] <- table7[which(x > caps[,2]), p_fl_high]

  propVect <- cbind(pstem = pstem, pbark = pbark, pbranches = pbranches, pfol = pfol)

  if(any(abs(rowSums(propVect) - 1) > 0.001)) {
    stop("The sums of biomass proportions do not sum to 1...")
  }

  return(propVect)
}
