boudewynSubsetTables <- function(table, thisAdmin, eco) {
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
  ecoNotInT <- c(8, 11, 15, 16, 17, 18)
  EcoBoundaryID <- c(7, 4, 6, 5, 6, 10)
  ecoReplace <- data.table(ecoNotInT, EcoBoundaryID)
  # these are the provinces available: AB BC NB NF NT
  # for the non match these would be the equivalent
  # "PE" - NB
  # "QC" - NL
  # "ON" - NL
  # "MB" - AB
  # "SK" - AB
  # "YK" - NT
  # "NU" - NT
  # "NS" - NB
  abreviation <- c("PE", "QC", "ON", "MB", "SK", "YK", "NU", "NS")
  tabreviation <- c("NB", "NL", "NL", "AB", "AB", "NT", "NT", "NB")
  abreviationReplace <- data.table(abreviation, tabreviation)
  if (any(eco %in% ecoNotInT)) { #if the study area is in ecozones not in the tables
    thisAdmin <- merge(ecoReplace, thisAdmin, by.x = "ecoNotInT", by.y = "EcoBoundaryID")
    smallTable <- as.data.table(table[table$juris_id %in% thisAdmin$abreviation &
                                      table$ecozone %in% thisAdmin$EcoBoundaryID, ])
  } else if (any(thisAdmin$abreviation %in% abreviation)) { #if the study area is in a province not in the tables
    thisAdminT <- merge(abreviationReplace, thisAdmin)
    thisAdminT[, c("abreviation", "tabreviation") := list(tabreviation, NULL)]
    smallTable <- as.data.table(table[table$juris_id %in% thisAdminT$abreviation &
                                        table$ecozone %in% eco, ])
  } else {
    smallTable <- as.data.table(table[table$juris_id %in% thisAdmin$abreviation &
                                        table$ecozone %in% eco, ])
  }
  return(smallTable)
  }
