
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Set table of disturbance events
.distEventsAgeAdjustTest <- function(){
  as.data.frame(rbind(

    c(id = 1, year = 1985),
    c(id = 1, year = 1995),
    c(id = 1, year = 2005),
    c(id = 1, year = 2009),

    c(id = 1, year = 2000),
    c(id = 2, year = 2007),
    c(id = 2, year = 2020),
    c(id = 2, year = 2025),

    c(id = 3, year = 1995),
    c(id = 3, year = 1997),
    c(id = 3, year = 2015),

    c(id = 4, year = 1990)
  ))
}

test_that("adjustCohortAges without disturbances", {

  # Test: step ages forward in time without disturbances
  agesAdjust <- adjustCohortAges(
    yearInput = 2000, yearOutput = 2010,
    cohortAges = data.frame(pixelIndex = 1:5, age = c(1:4, NA))
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(pixelIndex = 1:5, age = c(11:14, NA)))

  # Test: step ages backwards in time without disturbances
  agesAdjust <- adjustCohortAges(
    yearInput = 2000, yearOutput = 1990,
    cohortAges = data.frame(pixelIndex = 1:5, age = c(11:14, NA))
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(pixelIndex = 1:5, age = c(1:4, NA)))

  # Test: step back further than the cohort ages without a default
  agesAdjust <- expect_warning(
    adjustCohortAges(
      yearInput = 2000, yearOutput = 1987,
      cohortAges = data.frame(pixelIndex = 1:5, age = c(11:14, NA)),
    ))
  expect_equal(as.data.frame(agesAdjust), data.frame(pixelIndex = 1:5, age = c(NA, NA, 0, 1, NA)))

  # Test: step back further than the cohort ages with a default in a column
  agesAdjust <- adjustCohortAges(
    yearInput = 2000, yearOutput = 1987,
    cohortAges = data.frame(pixelIndex = 1:5, age = c(11:14, NA), default = 1:5)
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(pixelIndex = 1:5, age = c(1, 2, 0, 1, NA)))

})

test_that("adjustCohortAges forward with disturbances", {

  disturbanceEvents <- .distEventsAgeAdjustTest()

  agesAdjust <- adjustCohortAges(
    yearInput = 2000, yearOutput = 2010,
    cohortAges = data.frame(id = 1:6, age = c(rep(20, 5), NA)),
    disturbanceEvents = disturbanceEvents
  )
  expect_equal(as.data.frame(agesAdjust), as.data.frame(rbind(
    c(id = 1, age = 1),  # disturbance 2009
    c(id = 2, age = 3),  # disturbance 2007
    c(id = 3, age = 30), # disturbance N/A (outside range)
    c(id = 4, age = 30), # disturbance N/A (outside range)
    c(id = 5, age = 30), # disturbance N/A
    c(id = 6, age = NA)  # input NA, output NA
  )))

  # Check adding a regeneration delay
  agesAdjust <- adjustCohortAges(
    yearInput = 2000, yearOutput = 2010,
    cohortAges = data.frame(id = 1:6, age = c(rep(20, 5), NA)),
    disturbanceEvents = disturbanceEvents,
    delay = 2
  )
  expect_equal(as.data.frame(agesAdjust), as.data.frame(rbind(
    c(id = 1, age = 0),  # disturbance 2009
    c(id = 2, age = 1),  # disturbance 2007
    c(id = 3, age = 30), # disturbance N/A (outside range)
    c(id = 4, age = 30), # disturbance N/A (outside range)
    c(id = 5, age = 30), # disturbance N/A
    c(id = 6, age = NA)  # input NA, output NA
  )))

  ## Check for same result if 'delay' is a column
  expect_equal(
    agesAdjust,
    suppressWarnings(adjustCohortAges(
      yearInput = 2000, yearOutput = 2010,
      cohortAges = data.frame(id = 1:6, age = c(rep(20, 5), NA), delay = 2),
      disturbanceEvents = disturbanceEvents
    )))
})

test_that("adjustCohortAges backwards with disturbances", {

  disturbanceEvents <- .distEventsAgeAdjustTest()

  ## Expect warning: cohort 3 could not be calculated.
  ## It was disturbed before the input date, but there's no
  ## event previous to this to indicate when the cohort began growing.
  agesAdjust <- expect_warning(
    adjustCohortAges(
      yearInput = 2000, yearOutput = 1990,
      cohortAges = data.frame(id = 1:6, age = c(rep(20, 5), NA)),
      disturbanceEvents = disturbanceEvents
    )
  )
  expect_equal(as.data.frame(agesAdjust), as.data.frame(rbind(
    c(id = 1, age = 5),  # disturbance 1985
    c(id = 2, age = 10), # disturbance N/A (outside range)
    c(id = 3, age = NA), # disturbance 1995 but none before
    c(id = 4, age = 0),  # disturbance 1990
    c(id = 5, age = 10), # disturbance N/A
    c(id = 6, age = NA)  # input NA, output NA
  )))

  ## Check using a default age when age is unknown
  agesAdjust <- adjustCohortAges(
    yearInput = 2000, yearOutput = 1990,
    cohortAges = data.frame(id = 1:6, age = c(rep(20, 5), NA)),
    disturbanceEvents = disturbanceEvents,
    default = 1000
  )
  expect_equal(as.data.frame(agesAdjust), as.data.frame(rbind(
    c(id = 1, age = 5),
    c(id = 2, age = 10),
    c(id = 3, age = 1000), # default age applied
    c(id = 4, age = 0),
    c(id = 5, age = 10),
    c(id = 6, age = NA)
  )))

  ## Check for same result if given as a column
  expect_equal(
    agesAdjust,
    adjustCohortAges(
      yearInput = 2000, yearOutput = 1990,
      cohortAges = data.frame(id = 1:6, age = c(rep(20, 5), NA), default = 1000),
      disturbanceEvents = disturbanceEvents
    ))

  # Check adding a regeneration delay
  agesAdjust <- expect_warning(
    adjustCohortAges(
      yearInput = 2000, yearOutput = 1990,
      cohortAges = data.frame(id = 1:6, age = c(rep(20, 5), NA), delay = 2),
      disturbanceEvents = disturbanceEvents
    )
  )
  expect_equal(as.data.frame(agesAdjust), as.data.frame(rbind(
    c(id = 1, age = 3),  # cohort delayed
    c(id = 2, age = 10),
    c(id = 3, age = NA),
    c(id = 4, age = 0),
    c(id = 5, age = 10),
    c(id = 6, age = NA)
  )))

  ## Check if regeneration delay exceeds potential growth time
  agesAdjust <- expect_warning(
    adjustCohortAges(
      yearInput = 2000, yearOutput = 1990,
      cohortAges = data.frame(id = 1:6, age = c(rep(20, 5), NA), delay = 8),
      disturbanceEvents = disturbanceEvents
    )
  )
  expect_equal(as.data.frame(agesAdjust), as.data.frame(rbind(
    c(id = 1, age = 0),  # cohort delayed
    c(id = 2, age = 10),
    c(id = 3, age = NA),
    c(id = 4, age = 0),
    c(id = 5, age = 10),
    c(id = 6, age = NA)
  )))

  ## Check warning: cohorts lost
  agesAdjust <- expect_warning(
    adjustCohortAges(
      yearInput = 2000, yearOutput = 1990,
      cohortAges        = data.frame(id = 1, age = 2),
      disturbanceEvents = data.frame(id = 1, year = 2009)
    ))
  expect_equal(as.data.frame(agesAdjust), as.data.frame(rbind(
    c(id = 1, age = NA)
  )))

  agesAdjust <- adjustCohortAges(
    yearInput = 2000, yearOutput = 1990,
    cohortAges        = data.frame(id = 1, age = 2, default = 10),
    disturbanceEvents = data.frame(id = 1, year = 2009)
  )
  expect_equal(as.data.frame(agesAdjust), as.data.frame(rbind(
    c(id = 1, age = 10)
  )))

  ## Check warning: disturbances indicate that cohort ages are too high
  ## Age returned ignores disturbance event
  agesAdjust <- expect_warning(
    adjustCohortAges(
      yearInput = 2000, yearOutput = 1997,
      cohortAges        = data.frame(id = 1, age = 20),
      disturbanceEvents = data.frame(id = 1, year = c(1985, 1995))
    ))
  expect_equal(as.data.frame(agesAdjust), as.data.frame(rbind(
    c(id = 1, age = 17)
  )))
})


