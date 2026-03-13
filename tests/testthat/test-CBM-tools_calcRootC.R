
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("calcRootC", {
  ABC <- data.table(Merch = c(0,1),
                    Foliage = c(3,1),
                    Other = c(1,1))
  sw_hw = c(0,1)
  rootC <- calcRootC(aboveGroundC = ABC, sw_hw = sw_hw)

  # Works correctly
  expect_identical(
    round(rootC,3),
    data.table(coarseRoots = c(0.542, 1.570),
               fineRoots = c(0.346, 0.802))
  )
  expect_named(rootC, c("coarseRoots", "fineRoots"))

  # Error with missspecified inputs
  expect_error(
    calcRootC(aboveGroundC = ABC[,.(Merch, Foliage, other = Other)], sw_hw = sw_hw)
  )

  expect_error(
    calcRootC(aboveGroundC = ABC[,.(Merch, Foliage)], sw_hw = sw_hw)
  )

  expect_error(
    calcRootC(aboveGroundC = ABC, sw_hw = c("sw", "hw"))
  )

})
