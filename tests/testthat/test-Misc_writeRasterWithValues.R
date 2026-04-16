
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Function: writeRasterWithValues", {

  tempDir <- file.path(testDirs$temp$outputs, "writeRasterWithValues")
  dir.create(tempDir, recursive = TRUE, showWarnings = FALSE)

  templateRast <- terra::rast(
    crs = file.path(testDirs$testdata, "CRS", "EPSG-32613.prj"),
    res = 100, vals = 1,
    ext = c(xmin =  456000,  xmax = 457000, ymin = 6105000, ymax = 6106000))

  outPath <- file.path(tempDir, "writeRasterWithValues-numeric.tif")
  writeRasterWithValues(
    templateRast, values = c(rep(1, 50), rep(2, 50)),
    filename = outPath, overwrite = TRUE)

  rVals <- terra::rast(outPath)
  expect_true(terra::compareGeom(rVals, templateRast, stopOnError = FALSE))
  expect_equal(terra::values(rVals)[, 1], c(rep(1, 50), rep(2, 50)))
  expect_true(is.null(terra::cats(rVals)[[1]]))

  outPath <- file.path(tempDir, "writeRasterWithValues-text.tif")
  writeRasterWithValues(
    templateRast, values = c(rep("Hello 1", 50), rep("Goodbye 2", 50)),
    filename = outPath, overwrite = TRUE)

  rVals <- terra::rast(outPath)
  expect_true(terra::compareGeom(rVals, templateRast, stopOnError = FALSE))
  expect_equal(terra::values(rVals)[, 1], c(rep(2, 50), rep(1, 50)))
  expect_equal(terra::cats(rVals)[[1]][[1]],  1:2)
  expect_equal(terra::cats(rVals)[[1]][[2]],  c("Goodbye 2", "Hello 1"))

})
