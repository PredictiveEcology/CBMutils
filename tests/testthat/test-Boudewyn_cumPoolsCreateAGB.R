
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

table6AGB <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv",
                                      fun = "data.table::fread",
                                      destinationPath = testDirs$temp$inputs,
                                      filename2 = "appendix2_table6_tb.csv")
table7AGB <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv",
                                      fun = "data.table::fread",
                                      destinationPath = testDirs$temp$inputs,
                                      filename2 = "appendix2_table7_tb.csv")
tableMerchAGB <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1wa2QMd7Eo-bPpfigchdpPPPxo7NVpPiC/view?usp=drive_link",
                                          fun = "data.table::fread",
                                          destinationPath = testDirs$temp$inputs,
                                          filename2 = "appendix2_table7_tb.csv")
tableMerchAGB <- cbind(tableMerchAGB, minAge = 15)

test_that("getParameters", {
  # 4 curves:
  #   1) 1 exact match
  #   2) species and ecozone match
  #   3) species and jurisdiction match
  #   4) species only match
  x <- data.table(canfi_species = c(101, 204, 204, 302),
                  ecozone = c(4, 6, -999, -999),
                  juris_id = c("BC", "NA", "BC", "NA"))
  out <- getParameters(table6AGB, table7AGB, tableMerchAGB, x)
  expected_c2 <- c(0.0012709, 0.0027023, 0.0009288, -0.0038565)
  expected_p_sw_high <- c(0.757342072, 0.789843375, 0.817886188, 0.735608972)
  expected_cap <- c(0.11380327191837, 0.422873035, 0.266752439, 0.236493358)
  expect_is(out, "data.table")
  expect_named(out, c("canfi_species", "ecozone", "juris_id",
                      "a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3",
                      "biom_min", "biom_max", "p_sw_low", "p_sb_low", "p_br_low",
                      "p_fl_low", "p_sw_high", "p_sb_high", "p_br_high", "p_fl_high",
                      "a", "b", "k", "cap", "minAge"))
  expect_equal(out[,c("canfi_species", "ecozone", "juris_id")], x)

  # Check that matches are ok
  expect_equal(out$c2, expected_c2)
  expect_equal(out$p_sw_high, expected_p_sw_high)
  expect_equal(out$cap, expected_cap)

  # Check that the function errors when there are no parameters for a species
  expect_error(getParameters(table6AGB, table7AGB, tableMerchAGB, 1, 4, "BC"))
})

test_that("convertAGB2pools", {
  dt <- data.table(
    expand.grid(canfi_species = c(204), # PINU_CON
                age = c(3, 15, 35),
                ecozone = 4,
                juris_id = "AB"
    )
  )
  dt$B <- c(50, 100, 200)
  params <- getParameters(table6AGB, table7AGB, tableMerchAGB, data.table(canfi_species = 204, ecozone = 4, juris_id = "AB"))

  out <- convertAGB2pools(dt, params)

  # Sum of the pools equal total AGB
  expect_equal(rowSums(out), dt$B)

  # First line is under minimum age (merch should be 0), and under the biomass cap
  expected_result = data.table(merch = 0,
                               foliage = params$p_fl_low * dt$B[1],
                               other = dt$B[1] * (1-params$p_fl_low))
  expect_equal(out[1, ], expected_result)
  # Over the maximum cap
  expect_equal(out$foliage[3], dt$B[3] * params$p_fl_high)

  # Check output structure
  expect_true(all(colnames(out) == c("merch", "foliage", "other")))
  expect_true(all(!is.na(out)))
  expect_equal(dim(out), c(3,3))

})

test_that("cumPoolsCreateAGB", {
  # Test simple example
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
  out2 <- cumPoolsCreateAGB(dt, table6 = table6AGB, table7 = table7AGB, tableMerchantability = tableMerchAGB, pixGroupCol ="poolsPixelGroup")

  expect_equal(rowSums(out2[,c("merch", "foliage", "other")]), dt$B/2)
  expect_true(all(out2[dt$age < 15, "merch"] ==  0))
  expect_equal(nrow(out2), nrow(dt))
  expect_true(all(colnames(out2) == c("speciesCode", "age", "poolsPixelGroup", "merch", "foliage", "other")))

  # test with large data.table
  N <- 10^5
  dt <- data.table(
    canfi_species = sample(table6AGB$canfi_spec, N, replace = T),
    ecozone = sample(table6AGB$ecozone, N, replace = T),
    juris_id = sample(table6AGB$juris_id, N, replace = T),
    poolsPixelGroup = sample(c(1:10), N, replace = T),
    age = sample(c(1:500), N, replace = T),
    B = round(runif(N, 1, 100))
  )
  dt$speciesCode <- "as"
  out <- cumPoolsCreateAGB(dt, table6 = table6AGB, table7 = table7AGB, tableMerchantability = tableMerchAGB, pixGroupCol ="poolsPixelGroup")
  expect_equal(rowSums(out[,c("merch", "foliage", "other")]), dt$B/2)
  expect_true(all(out[dt$age < 15, "merch"] ==  0))
  expect_equal(nrow(out), nrow(dt))
  expect_true(all(colnames(out) == c("speciesCode", "age", "poolsPixelGroup", "merch", "foliage", "other")))


})

