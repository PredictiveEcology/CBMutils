
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("ageStepBackward: without disturbances", {

  ageVals <- c(10:15, rep(NA, 3))

  ageRast <- terra::rast(
    crs  = "EPSG:3979",
    ext  = c(xmin = -681090, xmax = -681000, ymin = 711900, ymax = 711990),
    res  = 30,
    vals = ageVals
  )

  # Expect error: yearOut > yearIn
  expect_error(ageStepBackward(ageRast, yearIn = 2025, yearOut = 2026))

  # Check that ages do not change
  ageRastStep <- ageStepBackward(ageRast, yearIn = 2025, yearOut = 2025)
  expect_equal(terra::values(ageRastStep, mat = FALSE), ageVals)

  # Check that ages step backwards
  ageRastStep <- ageStepBackward(ageRast, yearIn = 2025, yearOut = 2020)
  expect_equal(terra::values(ageRastStep, mat = FALSE), ageVals - 5)

})

test_that("ageStepBackward: with disturbances", {

  ageVals <- c(0, rep(10, 5), rep(NA, 3))

  ageRast <- terra::rast(
    crs  = "EPSG:3979",
    ext  = c(xmin = -681090, xmax = -681000, ymin = 711900, ymax = 711990),
    res  = 30,
    vals = ageVals
  )

  distEvents <- data.frame(
    pixelIndex = c(1, 1),
    year       = c(2024, 2025)
  )

  # Expect error: yearOut > yearIn
  expect_error(ageStepBackward(ageRast, yearIn = 2025, yearOut = 2026, distEvents = distEvents))

  # Check that ages do not change
  ageRastStep <- ageStepBackward(ageRast, yearIn = 2025, yearOut = 2025, distEvents = distEvents)
  expect_equal(terra::values(ageRastStep, mat = FALSE), ageVals)

  # Check that ages step backwards
  ageRastStep <- ageStepBackward(ageRast, yearIn = 2025, yearOut = 2020, distEvents = distEvents)
  expect_equal(
    terra::values(ageRastStep, mat = FALSE),
    c(rep(5, 6), rep(NA, 3))
  )

  # Check ignoring values <= 0
  ageRastStep <- ageStepBackward(
    terra::rast(
      crs  = "EPSG:3979",
      ext  = c(xmin = -681090, xmax = -681000, ymin = 711900, ymax = 711990),
      res  = 30,
      vals = c(0, 10, -1, 0, 10, 10, rep(NA, 3))
    ),
    yearIn = 2025, yearOut = 2020, distEvents = distEvents)
  expect_equal(
    terra::values(ageRastStep, mat = FALSE),
    c(rep(5, 6), rep(NA, 3))
  )
})

