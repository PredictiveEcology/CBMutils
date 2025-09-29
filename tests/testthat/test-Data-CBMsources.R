
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("CBMsourcePrepInputs", {

  inputPath <- file.path(testDirs$temp$outputs, "CBMsourcePrepInputs")

  srcCBM <- CBMsourcePrepInputs("CanSIS-ecozone", inputPath = inputPath)
  expect_is(srcCBM, "list")
  expect_equal(srcCBM$attr, "ecozone")
  expect_is(srcCBM$source, "sf")

  ## Backup test
  # srcCBM <- CBMsourcePrepInputs("StatCan-admin", inputPath = inputPath)
  # expect_is(srcCBM, "list")
  # expect_equal(srcCBM$attr, "admin")
  # expect_is(srcCBM$source, "sf")
  # expect_equal(names(srcCBM$source), c("admin", "geometry"))
})

test_that("CBMsourceExtractToRast", {

  inputPath <- file.path(testDirs$temp$outputs, "CBMsourcePrepInputs")

  templateRasts <- list(
    BC = terra::rast(
      crs  = "EPSG:102001",
      res  = 250,
      vals = 1L,
      xmin = ((-1632758.351 - -1684934.036)/2 + -1684934.036) - (250 * 100),
      xmax = ((-1632758.351 - -1684934.036)/2 + -1684934.036) + (250 * 100),
      ymin = ((2032247.399 - 1978635.729)/2 + 1978635.729) - (250 * 100),
      ymax = ((2032247.399 - 1978635.729)/2 + 1978635.729) + (250 * 100)
    )
  )

  srcCBMextr <- CBMsourceExtractToRast(
    "CanSIS-ecozone", inputPath = inputPath, templateRast = templateRasts$BC)

  expect_is(srcCBMextr, "list")
  expect_equal(srcCBMextr$extractToRast, rep(14L, 40000))

  ## Backup test
  # srcCBMextr <- CBMsourceExtractToRast(
  #   "StatCan-admin", inputPath = inputPath, templateRast = templateRasts$BC)
  #
  # expect_is(srcCBMextr, "list")
  # expect_equal(names(srcCBMextr), "admin")
  # expect_is(srcCBMextr[["admin"]], "character")
  # expect_equal(srcCBMextr[["admin"]], rep("British Columbia", 40000))

})

