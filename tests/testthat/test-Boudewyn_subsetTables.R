if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

table3 <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv",
                                      fun = "data.table::fread",
                                      destinationPath = testDirs$temp$inputs,
                                      filename2 = "appendix2_table3.csv")
eco <- c("9")

thisAdmin <- data.table(
  AdminBoundaryID = 9,
  stump_parameter_id = 9,
  adminName = "Saskatchewan",
  abreviation = "SK",
  SpatialUnitID = 28,
  EcoBoundaryID = 9
)

test_that("boudewynSubsetTables", {
  out <- boudewynSubsetTable(table = table6, thisAdmin = ThisAdmin, eco = eco)

  expect_is(out, "data.table")
})
