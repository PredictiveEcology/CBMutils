
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("calcRootC", {

  # Columns type 1
  ABC <- data.table(sw      = c(TRUE, FALSE),
                    Merch   = c(0,1),
                    Foliage = c(3,1),
                    Other   = c(1,1))

  rootC <- calcRootC(aboveGroundC = ABC)

  expect_equal(
    rootC[, .(CoarseRoots = SoftwoodCoarseRoots + HardwoodCoarseRoots,
              FineRoots   = SoftwoodFineRoots   + HardwoodFineRoots)],
    data.table(CoarseRoots = c(0.542, 1.570),
               FineRoots   = c(0.346, 0.802)),
    tolerance = 0.001, scale = 1
  )

  # Columns type 2
  ABC <- data.table(SoftwoodMerch   = c(0,0),
                    SoftwoodFoliage = c(3,0),
                    SoftwoodOther   = c(1,0),
                    HardwoodMerch   = c(0,1),
                    HardwoodFoliage = c(0,1),
                    HardwoodOther   = c(0,1))

  rootC <- calcRootC(aboveGroundC = ABC)

  expect_equal(
    rootC,
    data.table(SoftwoodCoarseRoots = c(0.542, 0.000),
               HardwoodCoarseRoots = c(0.000, 1.570),
               SoftwoodFineRoots   = c(0.346, 0.000),
               HardwoodFineRoots   = c(0.000, 0.802)),
    tolerance = 0.001, scale = 1
  )

  # Error with miss specified inputs
  expect_error(
    calcRootC(aboveGroundC = ABC[,.(sw, Merch, Foliage)])
  )

  expect_error(
    calcRootC(aboveGroundC = ABC[,.(sw = c(0, 1), Merch, Foliage, Other)])
  )
  expect_error(
    calcRootC(aboveGroundC = ABC[,.(sw = c("sw", "hw"), Merch, Foliage, Other)])
  )
})

