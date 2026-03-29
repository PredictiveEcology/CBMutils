
#' `CBMutils`
#'
#' Utilities for modelling carbon in R and SpaDES based on the Carbon Budget Model
#' of the Canadian Forest Service v3 (CBM-CFS3).
#'
#' @keywords internal
#' @import methods
"_PACKAGE"


# data.table package common variables
utils::globalVariables(c(".", ":=", ".BY", ".N", ".SD", ".GRP"))

# Common spatial variables
utils::globalVariables(c("x", "y", "z", "geometry", "area"))

# Common keys
utils::globalVariables(c("pixelIndex", "cohortID", "cohortGroupID", "year"))

# Boudewyn table columns
utils::globalVariables(c(
  "juris_id", "ecozone", "canfi_species", "genus", "species"
))

# CBM defaults database columns
utils::globalVariables(c(
  "id", "locale_id", "name", "description",
  "spatial_unit_id", "admin_boundary_id", "eco_boundary_id",
  "disturbance_type_id", "disturbance_matrix_id",
  "pool_id", "source_pool_id", "sink_pool_id", "code", "proportion"
))

# CBM-EXN cbm_vars columns
utils::globalVariables(c(
  "row_idx",

  # cbm_vars$state
  "area", "spatial_unit_id", "land_class_id", "age", "species", "sw_hw",
  "time_since_last_disturbance", "time_since_land_use_change", "last_disturbance_type",
  "mean_annual_temperature", "delay",

  # cbm_vars$flux
  "DisturbanceCO2Production", "DisturbanceCH4Production", "DisturbanceCOProduction",
  "DisturbanceBioCO2Emission", "DisturbanceBioCH4Emission", "DisturbanceBioCOEmission",
  "DecayDOMCO2Emission", "DisturbanceProduction", "DisturbanceDOMProduction",
  "DeltaBiomass_AG", "DeltaBiomass_BG",
  "TurnoverMerchLitterInput", "TurnoverFolLitterInput", "TurnoverOthLitterInput",
  "TurnoverCoarseLitterInput", "TurnoverFineLitterInput",
  "DecayVFastAGToAir", "DecayVFastBGToAir", "DecayFastAGToAir", "DecayFastBGToAir",
  "DecayMediumToAir", "DecaySlowAGToAir", "DecaySlowBGToAir",
  "DecayStemSnagToAir", "DecayBranchSnagToAir", "DisturbanceMerchToAir", "DisturbanceFolToAir",
  "DisturbanceOthToAir", "DisturbanceCoarseToAir", "DisturbanceFineToAir",
  "DisturbanceDOMCO2Emission", "DisturbanceDOMCH4Emission", "DisturbanceDOMCOEmission",
  "DisturbanceMerchLitterInput", "DisturbanceFolLitterInput", "DisturbanceOthLitterInput",
  "DisturbanceCoarseLitterInput", "DisturbanceFineLitterInput",
  "DisturbanceVFastAGToAir", "DisturbanceVFastBGToAir", "DisturbanceFastAGToAir",
  "DisturbanceFastBGToAir", "DisturbanceMediumToAir", "DisturbanceSlowAGToAir",
  "DisturbanceSlowBGToAir", "DisturbanceStemSnagToAir", "DisturbanceBranchSnagToAir",

  # cbm_vars$pools
  "Input", "Merch", "Foliage", "Other", "CoarseRoots", "FineRoots",
  "AboveGroundVeryFastSoil", "BelowGroundVeryFastSoil", "AboveGroundFastSoil", "BelowGroundFastSoil", "MediumSoil", "AboveGroundSlowSoil", "BelowGroundSlowSoil",
  "StemSnag", "BranchSnag", "CO2", "CH4", "CO", "NO2", "Products"
))


