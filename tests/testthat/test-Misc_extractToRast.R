
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

tempDir <- file.path(testDirs$temp$outputs, "extractToRast")
if (!testthat::is_testing()){
  file.copy(file.path(testDirs$testdata, "extractToRast"), testDirs$temp$outputs, recursive = TRUE, overwrite = TRUE)
}else dir.create(tempDir, recursive = TRUE, showWarnings = FALSE)

test_that("Function: writeRasterWithValues", {

  masterRaster <- terra::rast(
    res = 100, vals = 1, crs = "EPSG:32613",
    ext = c(xmin =  456000,  xmax = 457000, ymin = 6105000, ymax = 6106000))

  outPath <- file.path(tempDir, "writeRasterWithValues-numeric.tif")
  writeRasterWithValues(
    masterRaster, values = c(rep(1, 50), rep(2, 50)),
    filename = outPath, overwrite = TRUE)

  rVals <- terra::rast(outPath)
  expect_true(terra::compareGeom(rVals, masterRaster, stopOnError = FALSE))
  expect_equal(terra::values(rVals)[, 1], c(rep(1, 50), rep(2, 50)))
  expect_true(is.null(terra::cats(rVals)[[1]]))

  outPath <- file.path(tempDir, "writeRasterWithValues-text.tif")
  writeRasterWithValues(
    masterRaster, values = c(rep("Hello 1", 50), rep("Goodbye 2", 50)),
    filename = outPath, overwrite = TRUE)

  rVals <- terra::rast(outPath)
  expect_true(terra::compareGeom(rVals, masterRaster, stopOnError = FALSE))
  expect_equal(terra::values(rVals)[, 1], c(rep(2, 50), rep(1, 50)))
  expect_equal(terra::cats(rVals)[[1]][[1]],  1:2)
  expect_equal(terra::cats(rVals)[[1]][[2]],  c("Goodbye 2", "Hello 1"))

})

test_that("Function: extractToRast: raster upsampling", {

  input <- terra::rast(file.path(testDirs$testdata, "extractToRast", "SaskDist_1987_crop.tif"))

  masterRaster <- terra::rast(
    res = 5, vals = 1, crs = "EPSG:3979",
    ext = c(xmin = -674500, xmax = -671500, ymin =  702000, ymax =  705000))

  alignVals <- extractToRast(input, masterRaster)

  if (!testthat::is_testing()) writeRasterWithValues(masterRaster, alignVals, file.path(tempDir, "rast-upsample.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(masterRaster))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(2, 3, 5, NA),
      N   = c(27859, 3091, 12723, 316327)
    ), tolerance = 10, scale = 1)

  # Check temporary directories
  expect_equal(length(list.files(file.path(tempdir(), "CBMutils"))), 0)
  expect_true(basename(dirname(terra::terraOptions(print = FALSE)[["tempdir"]])) != "CBMutils")
  expect_true(
    is.null(getOption("rasterTmpDir")) || basename(dirname(getOption("rasterTmpDir"))) != "CBMutils"
  )
})

test_that("Function: extractToRast: raster upsampling with categories", {

  input <- terra::rast(file.path(testDirs$testdata, "extractToRast", "SaskDist_1987_crop.tif"))

  masterRaster <- terra::rast(
    res = 5, vals = 1, crs = "EPSG:3979",
    ext = c(xmin = -674500, xmax = -671500, ymin =  702000, ymax =  705000))

  levels(input) <- data.frame(id = 1:5, cat = LETTERS[1:5])

  alignVals <- extractToRast(input, masterRaster)

  if (!testthat::is_testing()) writeRasterWithValues(masterRaster, alignVals, file.path(tempDir, "rast-upsample-cats.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(masterRaster))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c("B", "C", "E", NA),
      N   = c(27859, 3091, 12723, 316327)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: raster downsampling", {

  input <- terra::rast(file.path(testDirs$testdata, "extractToRast", "SaskDist_1987_crop.tif"))

  masterRaster <- terra::rast(
    res = 50, vals = 1, crs = "EPSG:3979",
    ext = c(xmin = -674500, xmax = -671500, ymin =  702000, ymax =  705000))

  alignVals <- extractToRast(input, masterRaster)

  if (!testthat::is_testing()) writeRasterWithValues(masterRaster, alignVals, file.path(tempDir, "rast-downsample.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(masterRaster))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(2, 3, 5, NA),
      N   = c(280, 32, 121, 3167)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: raster reprojecting", {

  input <- terra::rast(file.path(testDirs$testdata, "extractToRast", "tile1.tif"))

  masterRaster <- terra::rast(
    ncols = 213, nrows = 215,
    vals = 1, crs = "EPSG:4326",
    ext = c(xmin = -105.6567825386380974, xmax = -105.6294401406081107,
            ymin =   55.1008597705739831, ymax =   55.1284589047357017))

  alignVals <- extractToRast(input, masterRaster)

  if (!testthat::is_testing()) writeRasterWithValues(masterRaster, alignVals, file.path(tempDir, "rast-reproject.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(masterRaster))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(27, 28),
      N   = c(16369, 29426)
    ), tolerance = 50, scale = 1)
})

test_that("Function: extractToRast: TIF file", {

  input <- file.path(testDirs$testdata, "extractToRast", "tile1.tif")

  masterRaster <- terra::rast(
    res = 10, vals = 1, crs = "EPSG:32613",
    ext = c(xmin =  458500, xmax =  463500, ymin = 6105000, ymax = 6110000))

  alignVals <- extractToRast(input, masterRaster)

  if (!testthat::is_testing()) writeRasterWithValues(masterRaster, alignVals, file.path(tempDir, "rast-TIF.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(masterRaster))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(27, 28, NaN),
      N   = c(48548, 76452, 125000)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: TIF tiles", {

  input <- file.path(testDirs$testdata, "extractToRast", c("tile1.tif", "tile2.tif"))

  masterRaster <- terra::rast(
    res = 10, vals = 1, crs = "EPSG:32613",
    ext = c(xmin =  458500, xmax =  463500, ymin = 6105000, ymax = 6110000))

  alignVals <- extractToRast(input, masterRaster)

  if (!testthat::is_testing()) writeRasterWithValues(masterRaster, alignVals, file.path(tempDir, "rast-TIF-tiles.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(masterRaster))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(27, 28),
      N   = c(108952, 141048)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: sf polygons with numeric field", {

  input <- sf::st_read(
    file.path(testDirs$testdata, "extractToRast", "spuLocator.shp"), agr = "constant",
    quiet = TRUE)[, "id"]

  ## Create an NA area
  input <- subset(input, id != 8)

  masterRaster <- terra::rast(
    res = 100, vals = 1, crs = terra::crs(input),
    ext = round(terra::ext(input)))

  alignVals <- extractToRast(input, masterRaster)

  if (!testthat::is_testing()) writeRasterWithValues(masterRaster, alignVals, file.path(tempDir, "sf-numeric.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(masterRaster))
  expect_is(alignVals, "numeric")
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(1, 2, 3, 4, 5, 6, 7, NaN),
      N   = c(58876, 17794, 1939, 161340, 26220, 26934, 49538, 22779)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: sf polygons with numeric field: reproject", {

  input <- sf::st_read(
    file.path(testDirs$testdata, "extractToRast", "spuLocator.shp"), agr = "constant",
    quiet = TRUE)[, "id"]

  ## Create an NA area
  input <- subset(input, id != 8)

  masterRaster <- terra::rast(
    res = 10, vals = 1, crs = "EPSG:32613",
    ext = c(xmin =  456000,  xmax = 461000, ymin = 6105000, ymax = 6110000))

  alignVals <- extractToRast(input, masterRaster)

  if (!testthat::is_testing()) writeRasterWithValues(masterRaster, alignVals, file.path(tempDir, "sf-numeric-reproject.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(masterRaster))
  expect_is(alignVals, "numeric")
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(1, 4, 5, NaN),
      N   = c(69052, 116674, 19458, 44816)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: sf polygons with text field: reproject", {

  input <- sf::st_read(
    file.path(testDirs$testdata, "extractToRast", "spuLocator.shp"), agr = "constant",
    quiet = TRUE)[, "id"]
  input$id <- paste("Id", input$id)

  masterRaster <- terra::rast(
    res = 10, vals = 1, crs = "EPSG:32613",
    ext = c(xmin =  456000,  xmax = 461000, ymin = 6105000, ymax = 6110000))

  alignVals <- extractToRast(input, masterRaster)

  if (!testthat::is_testing()) writeRasterWithValues(masterRaster, alignVals, file.path(tempDir, "sf-text-reproject.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(masterRaster))

  ## Make sure spaces and letter case are honored
  expect_is(alignVals, "character")
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c("Id 1", "Id 4", "Id 5", "Id 8"),
      N   = c(69052, 116674, 19458, 44816)
    ), tolerance = 10, scale = 1)
})

if (!testthat::is_testing()) shell.exec(file.path(tempDir, "extractToRast.qgz"))


