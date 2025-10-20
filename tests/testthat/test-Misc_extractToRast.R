
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

tempDir <- file.path(testDirs$temp$outputs, "extractToRast")
viewResults <- !testthat::is_testing()
if (viewResults){
  file.copy(file.path(testDirs$testdata, "extractToRast"), testDirs$temp$outputs, recursive = TRUE, overwrite = TRUE)
}else dir.create(tempDir, recursive = TRUE, showWarnings = FALSE)

test_that("Function: writeRasterWithValues", {

  templateRast <- terra::rast(
    res = 100, vals = 1, crs = "EPSG:32613",
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

test_that("Function: extractToRast: raster upsampling", {

  input <- terra::rast(file.path(testDirs$testdata, "extractToRast", "SaskDist_1987_crop.tif"))

  templateRast <- terra::rast(
    res = 5, vals = 1, crs = "EPSG:3979",
    ext = c(xmin = -674500, xmax = -671500, ymin =  702000, ymax =  705000))

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "rast-upsample.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))
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

  templateRast <- terra::rast(
    res = 5, vals = 1, crs = "EPSG:3979",
    ext = c(xmin = -674500, xmax = -671500, ymin =  702000, ymax =  705000))

  levels(input) <- data.frame(id = 1:5, cat = LETTERS[1:5])

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "rast-upsample-cats.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))
  expect_equal(
    data.table::data.table(val = as.character(alignVals))[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c("B", "C", "E", NA),
      N   = c(27859, 3091, 12723, 316327)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: raster downsampling", {

  input <- terra::rast(file.path(testDirs$testdata, "extractToRast", "SaskDist_1987_crop.tif"))

  templateRast <- terra::rast(
    res = 50, vals = 1, crs = "EPSG:3979",
    ext = c(xmin = -674500, xmax = -671500, ymin =  702000, ymax =  705000))

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "rast-downsample.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(2, 3, 5, NA),
      N   = c(280, 32, 121, 3167)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: raster disaggregation", {

  input <- terra::rast(file.path(testDirs$testdata, "extractToRast", "tile1.tif"))

  templateRast <- terra::rast(
    res = 2, vals = 1, crs = terra::crs(input),
    ext = terra::ext(input))

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "rast-disagg.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(27, 28),
      N   = c(1606850, 4643150)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: raster reprojecting", {

  input <- terra::rast(file.path(testDirs$testdata, "extractToRast", "tile1.tif"))

  templateRast <- terra::rast(
    ncols = 213, nrows = 215,
    vals = 1, crs = "EPSG:4326",
    ext = c(xmin = -105.6567825386380974, xmax = -105.6294401406081107,
            ymin =   55.1008597705739831, ymax =   55.1284589047357017))

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "rast-reproject.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(27, 28),
      N   = c(16369, 29426)
    ), tolerance = 50, scale = 1)
})

test_that("Function: extractToRast: TIF file", {

  input <- file.path(testDirs$testdata, "extractToRast", "tile1.tif")

  templateRast <- terra::rast(
    res = 10, vals = 1, crs = "EPSG:32613",
    ext = c(xmin =  458500, xmax =  463500, ymin = 6105000, ymax = 6110000))

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "rast-TIF.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(27, 28, NaN),
      N   = c(48548, 76452, 125000)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: TIF tiles", {

  input <- file.path(testDirs$testdata, "extractToRast", c("tile1.tif", "tile2.tif"))

  templateRast <- terra::rast(
    res = 10, vals = 1, crs = "EPSG:32613",
    ext = c(xmin =  458500, xmax =  463500, ymin = 6105000, ymax = 6110000))

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "rast-TIF-tiles.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(27, 28),
      N   = c(108952, 141048)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: coverage with NAs", {

  # Test: without reprojection
  inputTemplate <- terra::rast(
    res = 5, vals = 1,
    crs = "EPSG:3979", xmin = 0, ymin = 0, xmax = 10, ymax = 10)
  templateRast <- terra::rast(
    res = 10, vals = 1,
    crs = "EPSG:3979", xmin = 0, ymin = 0, xmax = 10, ymax = 10)

  ## 25% coverage NA
  input <- inputTemplate
  terra::values(input) <- c(1, 1, 1, NA)
  expect_equal(extractToRast(input, templateRast), 1)

  ## 50% coverage NA
  input <- inputTemplate
  terra::values(input) <- c(1, 1, NA, NA)
  expect_equal(extractToRast(input, templateRast), 1)

  ## 75% coverage NA
  input <- inputTemplate
  terra::values(input) <- c(1, NA, NA, NA)
  expect_in(extractToRast(input, templateRast), c(NaN, NA_real_))

  ## > 50% coverage NA in 1 pixel
  input <- terra::rast(
    res = 8, vals = c(NA, NA, 1, NA),
    crs = "EPSG:3979", xmin = 0, ymin = 0, xmax = 16, ymax = 16)
  expect_equal(extractToRast(input, templateRast), 1)

  # Test: with reprojection
  inputTemplate <- terra::rast(
    crs = "EPSG:3979",
    res = 100, vals = 1,
    xmin = -663200,
    xmax = -663000,
    ymin =  729200,
    ymax =  729400
  )
  templateRast <- terra::rast(
    #crs = "EPSG:102001"
    crs = paste(c(
      "PROJCS[\"Canada_Albers_Equal_Area_Conic\"",
      "GEOGCS[\"NAD83\"", "DATUM[\"North_American_Datum_1983\"", "SPHEROID[\"GRS 1980\",6378137,298.257222101", "AUTHORITY[\"EPSG\",\"7019\"]]", "AUTHORITY[\"EPSG\",\"6269\"]]",
      "PRIMEM[\"Greenwich\",0", "AUTHORITY[\"EPSG\",\"8901\"]]", "UNIT[\"degree\",0.0174532925199433", "AUTHORITY[\"EPSG\",\"9122\"]]", "AUTHORITY[\"EPSG\",\"4269\"]]",
      "PROJECTION[\"Albers_Conic_Equal_Area\"]",
      "PARAMETER[\"latitude_of_center\",40]", "PARAMETER[\"longitude_of_center\",-96]", "PARAMETER[\"standard_parallel_1\",50]",
      "PARAMETER[\"standard_parallel_2\",70]", "PARAMETER[\"false_easting\",0]", "PARAMETER[\"false_northing\",0]",
      "UNIT[\"metre\",1", "AUTHORITY[\"EPSG\",\"9001\"]]", "AXIS[\"Easting\",EAST]", "AXIS[\"Northing\",NORTH]", "AUTHORITY[\"ESRI\",\"102001\"]]"
    ), collapse = ","),
    res = 100, vals = 1,
    xmin = -607300,
    xmax = -607200,
    ymin = 1712100,
    ymax = 1712200
  )

  ## < 50% coverage NA
  input <- inputTemplate
  terra::values(input) <- c(NA, NA, 1, 1)
  expect_in(extractToRast(input, templateRast), c(NaN, NA_real_))

  ## > 50% coverage NA
  input <- inputTemplate
  terra::values(input) <- c(1, 1, NA, NA)
  expect_equal(extractToRast(input, templateRast), 1)

  ## > 50% coverage NA in 1 large area
  input <- terra::rast(
    crs = "EPSG:3979",
    res = 100, vals = c(1, NA, NA, NA),
    xmin = -663200 + 50,
    xmax = -663000 + 50,
    ymin =  729200,
    ymax =  729400
  )
  expect_equal(extractToRast(input, templateRast), 1)

})

test_that("Function: extractToRast: sf polygons with numeric field", {

  input <- sf::st_read(
    file.path(testDirs$testdata, "extractToRast", "spuLocator.shp"), agr = "constant",
    quiet = TRUE)[, "id"]

  ## Create an NA area
  input <- subset(input, id != 8)

  templateRast <- terra::rast(
    res = 100, vals = 1, crs = terra::crs(input),
    ext = round(terra::ext(input)))

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "sf-numeric.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))
  expect_is(alignVals, "numeric")
  expect_equal(
    data.table::data.table(val = alignVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(1, 2, 3, 4, 5, 6, 7, NaN),
      N   = c(58876, 17794, 1939, 161340, 26220, 26934, 49538, 22779)
    ), tolerance = 10, scale = 1)
})

test_that("Function: extractToRast: sf polygons with non-unique values", {

  ## Check that the value assigned matches the value covering the greatest total area,
  ## not the value from individual polygon covering the greatest area.

  templateRast <- terra::rast(
    res = 10, vals = 1, crs = "local",
    xmin = 0, ymin = 0, xmax = 10, ymax = 10)

  input <- rbind(
    sf::st_sf(
      value = 1,
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(c(c(0, 0), c(0, 3), c(3, 3), c(3, 0), c(0, 0)), ncol = 2, byrow = TRUE))),
        crs = "local")),
    sf::st_sf(
      value = 1,
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(c(c(0, 6), c(0, 10), c(8, 10), c(8, 6), c(0, 6)), ncol = 2, byrow = TRUE))),
        crs = "local")),
    sf::st_sf(
      value = 2,
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(c(c(4, 0), c(4, 6), c(10, 6), c(10, 0), c(4, 0)), ncol = 2, byrow = TRUE))),
        crs = "local"))
  )

  alignVals <- extractToRast(input, templateRast)

  expect_equal(alignVals, 1)
})

test_that("Function: extractToRast: sf polygons with numeric field: reproject", {

  input <- sf::st_read(
    file.path(testDirs$testdata, "extractToRast", "spuLocator.shp"), agr = "constant",
    quiet = TRUE)[, "id"]

  ## Create an NA area
  input <- subset(input, id != 8)

  templateRast <- terra::rast(
    res = 10, vals = 1, crs = "EPSG:32613",
    ext = c(xmin =  456000,  xmax = 461000, ymin = 6105000, ymax = 6110000))

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "sf-numeric-reproject.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))
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

  templateRast <- terra::rast(
    res = 10, vals = 1, crs = "EPSG:32613",
    ext = c(xmin =  456000,  xmax = 461000, ymin = 6105000, ymax = 6110000))

  alignVals <- extractToRast(input, templateRast)

  if (viewResults) writeRasterWithValues(templateRast, alignVals, file.path(tempDir, "sf-text-reproject.tif"), overwrite = TRUE)

  expect_equal(length(alignVals), terra::ncell(templateRast))

  ## Make sure spaces and letter case are honored
  expect_is(alignVals, "factor")
  expect_equal(
    data.table::data.table(val = as.character(alignVals))[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c("Id 1", "Id 4", "Id 5", "Id 8"),
      N   = c(69052, 116674, 19458, 44816)
    ), tolerance = 10, scale = 1)
})

if (viewResults) shell.exec(file.path(tempDir, "extractToRast.qgz"))


