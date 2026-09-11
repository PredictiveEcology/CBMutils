utils::globalVariables(c(
  "i.ecozone_new", "i.juris_id_new"
))

#' Subset Boudewyn tables to fit study area
#'
#' @param table Boudewyn table to subset
#' @param thisAdmin Table defining study area with columns
#' `juris_id`: jurisdiction abbreviation(s); and
#' `ecozone`: ecozone ID(s).
#'
#' @return `smallTable` data.table
#'
#' @export
#' @importFrom data.table data.table
boudewynSubsetTables <- function(table, thisAdmin){

  thisAdmin <- data.table(thisAdmin)
  if (!"juris_id" %in% names(thisAdmin)) stop("thisAdmin requires column 'juris_id'")
  if (!"ecozone"  %in% names(thisAdmin)) stop("thisAdmin requires column 'ecozone'")

  # Set ecozone replacements
  # not all ecozones are in tables 3-7. There may be some mismatch here.
  # these are the ecozones in the tables
  # id               name
  # 4       Taiga Plains
  # 5  Taiga Shield West
  # 6 Boreal Shield West
  # 7  Atlantic Maritime
  # 9      Boreal Plains
  # 10  Subhumid Prairies
  # 12  Boreal Cordillera
  # 13   Pacific Maritime
  # 14 Montane Cordillera
  # these are the ones that are not.
  # id               name
  # 8   Mixedwood Plains  - 7  Atlantic Maritime
  # 11   Taiga Cordillera - 4 taiga plains
  # 15      Hudson Plains - 6 Boreal Shield West
  # 16  Taiga Shield East - 5  Taiga Shield West
  # 17 Boreal Shield East - 6 Boreal Shield West
  # 18  Semiarid Prairies - 10  Subhumid Prairies
  ecoReplace <- data.table(
    ecozone     = c( 8, 11, 15, 16, 17, 18),
    ecozone_new = c( 7,  4,  6,  5,  6, 10))

  # Replaces ecozones not in table with its appropriate replacement
  thisAdmin[ecoReplace, on = .(ecozone),
            ecozone := i.ecozone_new]

  # Set jurisdiction replacements
  # these are the provinces available: AB BC NB NL NT
  # for the non match these would be the equivalent
  # "PE" - NB
  # "QC" - NL/NB
  # "ON" - NL/NB
  # "MB" - AB
  # "SK" - AB
  # "YK" - NT/BC
  # "NU" - NT/NL
  # "NS" - NB
  jurisReplace <- data.table(
    juris_id     = c("PE", "MB", "SK", "NS"),
    juris_id_new = c("NB", "AB", "AB", "NB"))

  # Replaces QC and ON with appropriate replacements depending on ecozone
  if (thisAdmin[juris_id %in% c("QC", "ON") & ecozone %in% c(5, 6), .N] > 0  && !any(c("QC", "ON") %in% table$juris_id)) {
    thisAdmin[juris_id %in% c("QC", "ON") & ecozone %in% c(5, 6),
              juris_id := "NL"]
  }

  if (thisAdmin[juris_id %in% c("QC", "ON") & ecozone == 7, .N] > 0 && !any(c("QC", "ON") %in% table$juris_id)) {
    thisAdmin[juris_id %in% c("QC", "ON") & ecozone == 7,
              juris_id := "NB"]
  }

  if (thisAdmin[juris_id %in% c("NU") & ecozone == 5, .N] > 0 && !"NU" %in% table$juris_id) {
    thisAdmin[juris_id %in% c("NU") & ecozone == 5,
              juris_id := "NT"]
  }

  if (thisAdmin[juris_id %in% "NU" & ecozone == 6, .N] > 0 && !"NU" %in% table$juris_id) {
    thisAdmin[juris_id %in% c("NU") & ecozone == 6,
              juris_id := "NL"]
  }

  if (thisAdmin[juris_id %in% "YK" & ecozone %in% c(4, 12), .N] > 0 && !"YK" %in% table$juris_id) {
    thisAdmin[juris_id %in% c("YK") & ecozone %in% c(4, 12),
              juris_id := "NT"]
  }

  if (thisAdmin[juris_id %in% c("YK") & ecozone == 13, .N] > 0 && !"YK" %in% table$juris_id) {
    thisAdmin[juris_id %in% c("YK") & ecozone == 13,
              juris_id := "BC"]
  }

  # Replaces jursdiction not in table with appropriate replacement
  if (any(thisAdmin$juris_id %in% jurisReplace$juris_id) && !any(thisAdmin$juris_id %in% table$juris_id)) {
    thisAdmin[jurisReplace,
              on = .(juris_id),
              juris_id := i.juris_id_new]
  }

  # Subset table
  smallTable <- merge(
    thisAdmin[, .(juris_id, ecozone)], data.table(table),
    by = c("juris_id", "ecozone"))

  return(smallTable)
}
