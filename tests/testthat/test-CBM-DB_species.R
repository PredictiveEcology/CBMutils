
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("sppMatch", {

  sppEquiv <- data.table::fread(file.path(testDirs$testdata, "sppEquivalencies.csv"))

  speciesNames = c(

    # Latin_full = NA, EN_generic_short identical to EN_generic_full
    "Fir-Spruce",

    # All the same
    "Acer platanoides",    # Latin_full
    "Nor map",             # EN_generic_short
    "nor. map.",           # EN_generic_short near match
    "Norway maple",        # EN_generic_full
    " \"nOrWaY mApLe\",  " # EN_generic_full near match
  )

  # Match with species names
  sppTable <- sppMatch(
    species = speciesNames,
    sppEquiv = sppEquiv
  )
  expect_equal(sppTable$CBM_speciesID, c(35, rep(88, 5)))

  # Match with duplicated species names
  sppTableDup <- sppMatch(
    species = c(speciesNames, speciesNames),
    sppEquiv = sppEquiv
  )
  expect_identical(sppTableDup, rbind(sppTable, sppTable))

  # Match with alternate species names
  sppTable <- sppMatch(
    species = "Maybe a fir", otherNames = list("Maybe a fir" = "Fir-Spruce"),
    sppEquiv = sppEquiv
  )
  expect_equal(sppTable$CBM_speciesID, 35)

  # 0 matches
  sppTable <- sppMatch(
    species = c(),
    sppEquiv = sppEquiv
  )
  expect_equal(nrow(sppTable), 0)

  # Multiple matches but unique result
  sppTable <- sppMatch(
    species = speciesNames,
    return  = "CBM_speciesID",
    sppEquiv = rbind(
      sppEquiv,
      sppEquiv[sppEquiv$CBM_speciesID %in% 35,]
    ))
  expect_equal(sppTable$CBM_speciesID, c(35, rep(88, 5)))

  # Expect error: NAs in input
  expect_error(
    sppMatch(
      species = c(speciesNames, NA),
      sppEquiv = sppEquiv
    )
  )

  # Expect error: match to a column that doesn't exist
  expect_error(
    sppMatch(
      species = speciesNames,
      sppEquiv = sppEquiv[, .SD, .SDcols = c(
        "Latin_full", "CBM_speciesID", "Broadleaf")])
  )

  # Expect error: match not found
  expect_error(
    sppMatch(
      species = speciesNames,
      sppEquiv = sppEquiv[!sppEquiv$CBM_speciesID %in% 35,]
    ),
    "Fir-Spruce")

  # Expect error: multiple matches
  expect_error(
    sppMatch(
      species = speciesNames,
      sppEquiv = rbind(
        sppEquiv,
        sppEquiv[sppEquiv$CBM_speciesID %in% 35,]
      )),
    "Fir-Spruce")

  # Expect error: NAs found
  expect_error(
    sppMatch(
      species = speciesNames,
      return  = c("CBM_speciesID", "Broadleaf"),
      check   = TRUE,
      sppEquiv = cbind(
        sppEquiv[sppEquiv$CBM_speciesID %in% c(35, 88), .SD, .SDcols = !"CBM_speciesID"],
        CBM_speciesID = c(NA, 1))
      ),
    "Fir-Spruce.*CBM_speciesID")

  # Expect error: check NAs for a column that doesn't exist
  expect_error(
    sppMatch(
      species = speciesNames,
      return  = c("CBM_speciesID", "column_not_found"),
      check   = TRUE,
      sppEquiv = sppEquiv
    )
  )
})

test_that("sppMatch to a chosen column", {

  sppEquiv <- data.table::fread(file.path(testDirs$testdata, "sppEquivalencies.csv"))

  # Match with a specific column
  sppTable <- sppMatch(
    species = c(2201, 301),
    match   = "CanfiCode",
    sppEquiv = sppEquiv
  )
  expect_equal(sppTable$CBM_speciesID, c(122, 28))

  sppTable <- sppMatch(
    species = c("ulmu_ame", "abie_ama"),
    match   = "LandR",
    sppEquiv = sppEquiv
  )
  expect_equal(sppTable$CBM_speciesID, c(122, 28))

  sppTable <- sppMatch(
    species = c("ulmus americana", "abies amabilis"),
    match   = "Latin_full",
    sppEquiv = sppEquiv
  )
  expect_equal(sppTable$CBM_speciesID, c(122, 28))

  # Match with duplicated species names
  sppTableDup <- sppMatch(
    species = c("ulmus americana", "abies amabilis", "ulmus americana", "abies amabilis"),
    match   = "Latin_full",
    sppEquiv = sppEquiv
  )
  expect_identical(sppTableDup, rbind(sppTable, sppTable))

  # Match with alternate species names
  sppTable <- sppMatch(
    species = "Not sure", otherNames = list("Not sure" = "ulmus americana"),
    sppEquiv = sppEquiv
  )
  expect_equal(sppTable$CBM_speciesID, 122)

  # 0 matches
  sppTable <- sppMatch(
    species = c(),
    match   = "CanfiCode",
    sppEquiv = sppEquiv
  )
  expect_equal(nrow(sppTable), 0)

  # Multiple matches but unique result
  sppTable <- sppMatch(
    species = c(301, 2201),
    match   = "CanfiCode",
    return  = "CBM_speciesID",
    sppEquiv = rbind(
      sppEquiv,
      sppEquiv[sppEquiv$CanfiCode %in% 301,]
    ))
  expect_equal(sppTable$CBM_speciesID, c(28, 122))

  sppTable <- sppMatch(
    species = c(301, 2201),
    match   = "CanfiCode",
    return  = "Broadleaf",
    sppEquiv = rbind(
      sppEquiv,
      sppEquiv[sppEquiv$CanfiCode %in% 301,]
    ))
  expect_equal(sppTable$Broadleaf, c(FALSE, TRUE))

  # Expect error: NAs in input
  expect_error(
    sppMatch(
      species = c(NA, 2201),
      match   = "CanfiCode",
      sppEquiv = sppEquiv
    )
  )

  # Expect error: match to a column that doesn't exist
  expect_error(
    sppMatch(
      species = c(301, 2201),
      match   = "CanfiCode",
      sppEquiv = sppEquiv[, .SD, .SDcols = c(
        "Latin_full", "CBM_speciesID", "Broadleaf")])
  )

  # Expect error: match not found
  expect_error(
    sppMatch(
      species = c(301, 2201),
      match   = "CanfiCode",
      sppEquiv = sppEquiv[!sppEquiv$CanfiCode %in% 301,]
    ),
    "301")

  # Expect error: multiple matches
  expect_error(
    sppMatch(
      species = c(301, 2201),
      match   = "CanfiCode",
      sppEquiv = rbind(
        sppEquiv,
        sppEquiv[sppEquiv$CanfiCode %in% 301,]
      )),
    "301")

  # Expect error: NAs found
  expect_error(
    sppMatch(
      species = c(301, 2201),
      match   = "CanfiCode",
      return  = c("CBM_speciesID", "Broadleaf"),
      check   = TRUE,
      sppEquiv = cbind(
        sppEquiv[sppEquiv$CanfiCode %in% c(301, 2201), .SD, .SDcols = !"CBM_speciesID"],
        CBM_speciesID = c(NA, 1))
    ),
    "301.*CBM_speciesID")

  # Expect error: check NAs for a column that doesn't exist
  expect_error(
    sppMatch(
      species = c(301, 2201),
      match   = "CanfiCode",
      return  = c("CBM_speciesID", "column_not_found"),
      check   = TRUE,
      sppEquiv = sppEquiv
    )
  )
})


