
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("ageStepBackward: without disturbances", {

  ageRastTemplate <- terra::rast(
    crs  = "EPSG:3979",
    ext  = c(xmin = -681090, xmax = -681000, ymin = 711900, ymax = 711990),
    res  = 30)

  ageRast <- terra::rast(ageRastTemplate, vals = c(10:15, rep(NA, 3)))

  # Expect error: yearOut > yearIn
  expect_error(ageStepBackward(ageRast, yearIn = 2025, yearOut = 2026))

  # Check that ages do not change
  ageRastStep <- ageStepBackward(ageRast, yearIn = 2025, yearOut = 2025)
  expect_equal(terra::values(ageRastStep, mat = FALSE), terra::values(ageRast, mat = FALSE))

  # Check that ages step backwards
  ageRastStep <- ageStepBackward(ageRast, yearIn = 2025, yearOut = 2020)
  expect_equal(terra::values(ageRastStep, mat = FALSE), terra::values(ageRast, mat = FALSE) - 5)

  # Check filling values <= 0
  ageRastNeedsFill <- terra::rast(
    ageRastTemplate,
    vals = c(5, 10, -1, 0, 10, 10, rep(NA, 3))
  )

  ageRastStep <- ageStepBackward(ageRastNeedsFill, yearIn = 2025, yearOut = 2020)
  expect_equal(
    terra::values(ageRastStep, mat = FALSE),
    c(rep(5, 6), rep(NA, 3))
  )

  ## Check stepping backwards without filling
  ageRastStep <- ageStepBackward(ageRastNeedsFill, yearIn = 2025, yearOut = 2020, fill = FALSE)
  expect_equal(
    terra::values(ageRastStep, mat = FALSE),
    c(NA, 5, NA, NA, 5, 5, rep(NA, 3))
  )

  # Expect error: no cells >=0 to interpolate from
  expect_error(ageStepBackward(ageRast, yearIn = 2025, yearOut = 2010))
})

test_that("ageStepBackward: with disturbances", {

  ageRastTemplate <- terra::rast(
    crs  = "EPSG:3979",
    ext  = c(xmin = -681090, xmax = -681000, ymin = 711900, ymax = 711990),
    res  = 30)

  ageRast <- terra::rast(
    ageRastTemplate,
    vals = c(0, rep(10, 5), rep(NA, 3))
  )
  distEvents <- data.frame(
    pixelIndex = c(1, 1),
    year       = c(2024, 2025)
  )

  # Expect error: yearOut > yearIn
  expect_error(ageStepBackward(ageRast, yearIn = 2025, yearOut = 2026, distEvents = distEvents))

  # Check that ages do not change
  ageRastStep <- ageStepBackward(ageRast, yearIn = 2025, yearOut = 2025, distEvents = distEvents)
  expect_equal(terra::values(ageRastStep, mat = FALSE), terra::values(ageRast, mat = FALSE))

  # Check that ages step backwards and fill
  ageRastStep <- ageStepBackward(ageRast, yearIn = 2025, yearOut = 2020, distEvents = distEvents)
  expect_equal(
    terra::values(ageRastStep, mat = FALSE),
    c(rep(5, 6), rep(NA, 3))
  )

  # Check ignoring values <= 0
  ageRastStep <- ageStepBackward(
    terra::rast(
      ageRastTemplate,
      vals = c(20, 10, -1, 0, 10, 10, rep(NA, 3))
    ),
    yearIn = 2025, yearOut = 2020, distEvents = distEvents)
  expect_equal(
    terra::values(ageRastStep, mat = FALSE),
    c(rep(5, 6), rep(NA, 3))
  )

  # Check stepping backwards without filling
  ageRastStep <- ageStepBackward(ageRast, yearIn = 2025, yearOut = 2020, distEvents = distEvents,
                                 fill = FALSE)
  expect_equal(
    terra::values(ageRastStep, mat = FALSE),
    c(NA, rep(5, 5), rep(NA, 3))
  )
})


