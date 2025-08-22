
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

masterRaster <- terra::rast(matrix(rep(1,4), nrow = 2, ncol = 2))

cohortGroupKeep <- data.table::data.table(
  pixelIndex = c(1:4),
  cohortGroupID = c(1:4),
  `1` = c(4:1),
  `2` = c(1:4)
)
cbmPoolsTest <- data.table::data.table(
  simYear = c(rep(1,4), rep(2,4)),
  cohortGroupID = rep(c(1:4),2),
  N = 1L,
  Merch = 1:8,
  BranchSnag = 101:108
)
NPPTest <- data.table::data.table(
  simYear = rep(c(1:2), each = 4),
  cohortGroupID = rep(c(1:4), 2),
  NPP = c(c(1:4), c(4:1))
)

test_that("spatialPlot", {
  out <- spatialPlot(cbmPoolsTest, 1, masterRaster, cohortGroupKeep)
  expect_is(out, "ggplot")
  expect_equal(out$plot_env$plotM$totalCarbon, c(108, 106, 104, 102))
  out2 <- spatialPlot(cbmPoolsTest, 2, masterRaster, cohortGroupKeep)
  expect_is(out2, "ggplot")
  expect_equal(out2$plot_env$plotM$totalCarbon, c(110, 112, 114, 116))
})

test_that("carbonOutPlot", {

  emissionsProducts <- qs::qread(file.path(testDirs$testdata, "CBM_core_outputs/SK/emissionsProducts.qs"))

  out <- carbonOutPlot(emissionsProducts)
  expect_is(out, "ggplot")
})

test_that("NPPplot", {
  out <- NPPplot(
    NPP = NPPTest, year = 1,
    cohortGroupKeep = cohortGroupKeep,
    masterRaster = masterRaster
  )
  expect_is(out, "ggplot")
  expect_equal(out$plot_env$overallAvgNpp, 2.5)
  expect_equal(out$plot_env$plotMaster$NPP, c(4, 3, 2, 1))
})

test_that("barPlot", {

})

test_that("cTransfersAlluvial", {

})

