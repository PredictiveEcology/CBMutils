boudewynTableSubset <- function(table, thisAdmin, eco) {
  ecoNotInT <- c(8, 11, 15, 16, 17, 18)
  EcoBoundaryID <- c(7, 4, 6, 5, 6, 10)
  ecoReplace <- data.table(ecoNotInT, EcoBoundaryID)
  abreviation <- c("PE", "QC", "ON", "MB", "SK", "YK", "NU", "NS")
  tabreviation <- c("NB", "NL", "NL", "AB", "AB", "NT", "NT", "NB")
  abreviationReplace <- data.table(abreviation, tabreviation)
  if (any(eco %in% ecoNotInT)) {
    thisAdmin <- merge(ecoReplace, thisAdmin, by.x = "ecoNotInT", by.y = "EcoBoundaryID")
    smallTable <- as.data.table(table[table$juris_id %in% thisAdmin$abreviation &
                                      table$ecozone %in% thisAdmin$EcoBoundaryID, ])
  } else if (any(thisAdmin$abreviation %in% abreviation)) {
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
