
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("cTransfersAlluvial", {

  cTransfers <- qs::qread(file.path(testDirs$testdata, "cTransfers_SPU-27-28_wildfire.qs"))

  cTransfersPlot <- cTransfersAlluvial(cTransfers, distMatrixID = 371)

  expect_s3_class(cTransfersPlot, "ggplot")

  cTransfersPlot <- cTransfersAlluvial(cTransfers, distName = "wildfire", spuID = 28, sw =  TRUE)

  expect_s3_class(cTransfersPlot, "ggplot")
  expect_equal(unique(cTransfersPlot$data$disturbance_matrix_id), 371)

})

