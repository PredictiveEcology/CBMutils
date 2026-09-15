
#' Create `cumPools` data.table
#'
#' @param fullSpecies Study area species' names
#' @param gcMeta Growth curve metadata table
#' @param userGcM3 Table of growth curve volume by age
#' @param thisAdmin Ecozone and spatial unit information table for the study area
#' @template bTable3
#' @template bTable4
#' @template bTable5
#' @template bTable6
#' @template bTable7
#' @template bRateBiomassToCarbon
#'
#' @return `cumPools` data.table
#'
#' @export
#' @importFrom data.table data.table rbindlist
cumPoolsCreate <- function(fullSpecies, gcMeta, userGcM3, thisAdmin,
                           bTable3 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv",
                           bTable4 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv",
                           bTable5 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv",
                           bTable6 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv",
                           bTable7 = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv",
                           bRateBiomassToCarbon = 0.5){

  # Read Boudewyn parameters
  if (!is.data.table(bTable3)) bTable3 <- ifelse(is.data.frame(bTable3), as.data.table(bTable3), fread(bTable3))
  if (!is.data.table(bTable4)) bTable4 <- ifelse(is.data.frame(bTable4), as.data.table(bTable4), fread(bTable4))
  if (!is.data.table(bTable5)) bTable5 <- ifelse(is.data.frame(bTable5), as.data.table(bTable5), fread(bTable5))
  if (!is.data.table(bTable6)) bTable6 <- ifelse(is.data.frame(bTable6), as.data.table(bTable6), fread(bTable6))
  if (!is.data.table(bTable7)) bTable7 <- ifelse(is.data.frame(bTable7), as.data.table(bTable7), fread(bTable7))

  counter <- 0L
  cumBiomList <- list()
  for (i in 1:length(fullSpecies)) {
    # matching on species name
    speciesMeta <- gcMeta[species == fullSpecies[i], ]
    # for each species name, process one gcID at a time
    for (j in 1:NROW(unique(speciesMeta, on = "gcids"))) {
      counter <- counter + 1L

      meta <- speciesMeta[j, ]
      ecozone <- meta$ecozones
      id <- userGcM3$gcids[which(userGcM3$gcids ==  meta$gcids)][-1]
      ## IMPORTANT BOUDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ##
      age <- userGcM3[gcids == meta$gcids, Age]
      age <- age[which(age > 0)]
      # series of fncts results in curves of merch, foliage and other (SW or HW)

      cumBiom <- as.matrix(convertM3biom(
        meta     = meta,
        gCvalues = userGcM3,
        spsMatch = gcMeta,
        ecozones = thisAdmin,
        bTable3  = unique(bTable3),
        bTable4  = unique(bTable4),
        bTable5  = unique(bTable5),
        bTable6  = unique(bTable6),
        bTable7  = unique(bTable7)
      ))

      # going from tonnes of biomass/ha to tonnes of carbon/ha here
      cumBiom <- cumBiom * bRateBiomassToCarbon
      # calculating the increments per year for each of the three pools (merch,
      # foliage and other (SW or HW))
      # inc <- diff(cumBiom)
      # CBM processes half the growth before turnover and OvermatureDecline, and
      # half after.
      # names(outInputs$allProcesses)
      # [1] "Disturbance"       "Growth1"           "DomTurnover"       "BioTurnover"
      # [5] "OvermatureDecline" "Growth2"           "DomDecay"          "SlowDecay"
      # [9] "SlowMixing"
      cumBiomList[[counter]] <- data.table(id, age, cumBiom, ecozone, gcids = meta$gcids)

      # cumPools <- rbind(cumPools, cumBiom)
    }
  }
  cumPools <- rbindlist(cumBiomList)
  return(cumPools)
}
