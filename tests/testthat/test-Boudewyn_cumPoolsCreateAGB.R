
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

table6AGB <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv",
                                      fun = "data.table::fread",
                                      destinationPath = testDirs$temp$inputs,
                                      filename2 = "appendix2_table6_tb.csv")
table7AGB <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv",
                                      fun = "data.table::fread",
                                      destinationPath = testDirs$temp$inputs,
                                      filename2 = "appendix2_table7_tb.csv")

test_that("getParameters", {
  # 4 curves:
  #   1) 1 exact match
  #   2) species and ecozone match
  #   3) species and jurisdiction match
  #   4) species only match
  x <- data.table(canfi_species = c(101, 204, 204, 302),
                  ecozone = c(4, 6, -999, -999),
                  juris_id = c("BC", "NA", "BC", "NA"))
  out <- getParameters(table6AGB, table7AGB, x)
  expected_c2 <- c(0.0012709, 0.0027023, 0.0009288, -0.0038565)
  expected_p_sw_high <- c(0.757342072, 0.789843375, 0.817886188, 0.735608972)

  expect_is(out, "list")
  expect_named(out, c("params6", "params7"))
  expect_equal(out$params6[,c("canfi_species", "ecozone", "juris_id")], x)
  expect_equal(out$params7[,c("canfi_species", "ecozone", "juris_id")], x)

  # Check that matches are ok
  expect_equal(out$params6$c2, expected_c2)
  expect_equal(out$params7$p_sw_high, expected_p_sw_high)

  # Check that the function errors when there are no parameters for a species
  expect_error(getParameters(table6AGB, table7AGB, 1, 4, "BC"))
})

test_that("convertAGB2pools", {
  dt <- data.table(
    expand.grid(canfi_species = c(204), # PINU_CON
                age = c(3,15, 35),
                ecozone = 4,
                juris_id = "AB"
    )
  )
  dt$B <- round(runif(nrow(dt), 1, 100))
  params <- getParameters(table6AGB, table7AGB, data.table(canfi_species = 204, ecozone = 4, juris_id = "AB"))
  out <- convertAGB2pools(dt, params6 = params$params6, params7 = params$params7)

  # Sum of the pools equal total AGB
  expect_equal(rowSums(out), dt$B)
  # Merchantable is 0 for age under 15
  expect_true(out[dt$age < 15, "merch"] ==  0)
  # Check output structure
  expect_true(all(colnames(out) == c("merch", "foliage", "other")))
  expect_true(all(!is.na(out)))
  expect_equal(dim(out), c(3,3))

})

test_that("cumPoolsCreateAGB", {
  dt <- data.table(
    expand.grid(canfi_species = c(204, 1201), # PINU_CON, POPU_TRE
                age = c(3,15, 35),
                ecozone = 4,
                juris_id = "AB",
                poolsPixelGroup = c(1,2)
    )
  )
  dt$B <- round(runif(nrow(dt), 1, 100))
  dt$speciesCode[dt$canfi_species == 204] <- "PINU_CON"
  dt$speciesCode[dt$canfi_species == 1201] <- "POPU_TRE"
  data.table::setorder(dt, speciesCode, age, poolsPixelGroup)
  out2 <- cumPoolsCreateAGB(dt, table6 = table6AGB, table7 = table7AGB, pixGroupCol ="poolsPixelGroup")

  expect_equal(rowSums(out2[,c("merch", "foliage", "other")]), dt$B/2)
  expect_true(all(out2[dt$age < 15, "merch"] ==  0))
  expect_equal(nrow(out2), nrow(dt))
  expect_true(all(colnames(out2) == c("speciesCode", "age", "poolsPixelGroup", "merch", "foliage", "other")))
})

