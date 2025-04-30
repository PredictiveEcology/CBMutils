
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("sppMatch", {

  sppEquivalencies <- data.table::fread(file.path(testDirs$testdata, "sppEquivalencies.csv"))

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
    sppEquivalencies = sppEquivalencies
  )
  expect_equal(sppTable$CBM_speciesID, c(35, rep(88, 5)))

  # 0 matches
  sppTable <- sppMatch(
    species = c(),
    sppEquivalencies = sppEquivalencies
  )
  expect_equal(nrow(sppTable), 0)

  # Expect error: NAs in input
  expect_error(
    sppMatch(
      species = c(speciesNames, NA),
      sppEquivalencies = sppEquivalencies
    )
  )

  # Expect error: match to a column that doesn't exist
  expect_error(
    sppMatch(
      species = speciesNames,
      sppEquivalencies = sppEquivalencies[, .SD, .SDcols = c(
        "Latin_full", "CBM_speciesID", "Broadleaf")])
  )

  # Expect error: match not found
  expect_error(
    sppMatch(
      species = speciesNames,
      sppEquivalencies = sppEquivalencies[!sppEquivalencies$CBM_speciesID %in% 35,]
    ),
    "Fir-Spruce")

  # Expect error: multiple matches
  expect_error(
    sppMatch(
      species = speciesNames,
      sppEquivalencies = rbind(
        sppEquivalencies,
        sppEquivalencies[sppEquivalencies$CBM_speciesID %in% 35,]
      )),
    "Fir-Spruce")

  # Expect error: NAs found
  expect_error(
    sppMatch(
      species = speciesNames,
      checkNA = c("CBM_speciesID", "Broadleaf"),
      sppEquivalencies = cbind(
        sppEquivalencies[sppEquivalencies$CBM_speciesID %in% c(35, 88), .SD, .SDcols = !"CBM_speciesID"],
        CBM_speciesID = c(NA, 1))
      ),
    "Fir-Spruce.*CBM_speciesID")

  # Expect error: check NAs for a column that doesn't exist
  expect_error(
    sppMatch(
      species = speciesNames,
      checkNA = c("CanfiCode", "column_not_found"),
      sppEquivalencies = sppEquivalencies
    )
  )
})

test_that("sppMatch to a chosen column", {

  sppEquivalencies <- data.table::fread(file.path(testDirs$testdata, "sppEquivalencies.csv"))

  # Match with a specific column
  sppTable <- sppMatch(
    species  = c(301, 2201),
    matchCol = "CanfiCode",
    sppEquivalencies = sppEquivalencies
  )
  expect_equal(sppTable$CBM_speciesID, c(28, 122))

  sppTable <- sppMatch(
    species  = c("Abies amabilis", "Ulmus americana"),
    matchCol = "Latin_full",
    sppEquivalencies = sppEquivalencies
  )
  expect_equal(sppTable$CBM_speciesID, c(28, 122))

  # 0 matches
  sppTable <- sppMatch(
    species  = c(),
    matchCol = "CanfiCode",
    sppEquivalencies = sppEquivalencies
  )
  expect_equal(nrow(sppTable), 0)

  # Expect error: NAs in input
  expect_error(
    sppMatch(
      species  = c(NA, 2201),
      matchCol = "CanfiCode",
      sppEquivalencies = sppEquivalencies
    )
  )

  # Expect error: match to a column that doesn't exist
  expect_error(
    sppMatch(
      species  = c(301, 2201),
      matchCol = "CanfiCode",
      sppEquivalencies = sppEquivalencies[, .SD, .SDcols = c(
        "Latin_full", "CBM_speciesID", "Broadleaf")])
  )

  # Expect error: match not found
  expect_error(
    sppMatch(
      species  = c(301, 2201),
      matchCol = "CanfiCode",
      sppEquivalencies = sppEquivalencies[!sppEquivalencies$CanfiCode %in% 301,]
    ),
    "301")

  # Expect error: multiple matches
  expect_error(
    sppMatch(
      species  = c(301, 2201),
      matchCol = "CanfiCode",
      sppEquivalencies = rbind(
        sppEquivalencies,
        sppEquivalencies[sppEquivalencies$CanfiCode %in% 301,]
      )),
    "301")

  # Expect error: NAs found
  expect_error(
    sppMatch(
      species  = c(301, 2201),
      matchCol = "CanfiCode",
      checkNA  = c("CBM_speciesID", "Broadleaf"),
      sppEquivalencies = cbind(
        sppEquivalencies[sppEquivalencies$CanfiCode %in% c(301, 2201), .SD, .SDcols = !"CBM_speciesID"],
        CBM_speciesID = c(NA, 1))
    ),
    "301.*CBM_speciesID")

  # Expect error: check NAs for a column that doesn't exist
  expect_error(
    sppMatch(
      species  = c(301, 2201),
      matchCol = "CanfiCode",
      checkNA  = c("CanfiCode", "column_not_found"),
      sppEquivalencies = sppEquivalencies
    )
  )
})


