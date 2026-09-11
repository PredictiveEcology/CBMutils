
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

spadesCBMdb <- file.path(testDirs$testdata, "CBM_core_outputs/SK/spadesCBMdb")
masterRaster <- terra::rast(
  vals = 1, crs = "local",
  xmin = 0, ymin = 0,
  ncols = 1950, xmax = 1950 * 30,
  nrows = 1900, ymax = 1900 * 30)

test_that("plotEmissionsProducts", {

  emissionsProducts <- qs2::qd_read(file.path(testDirs$testdata, "CBM_core_outputs/SK/emissionsProducts.qs2"))

  out <- plotEmissionsProducts(emissionsProducts)
  expect_is(out, "ggplot")
})

test_that("plotPoolProportions", {

  pools <- rbind(
    cbind(year = 1985, merge(
      qs2::qd_read(file.path(spadesCBMdb, "data", "1985_key.qs2")),
      qs2::qd_read(file.path(spadesCBMdb, "data", "1985_pools.qs2")),
      by = "row_idx")),
    cbind(year = 2011, merge(
      qs2::qd_read(file.path(spadesCBMdb, "data", "2011_key.qs2")),
      qs2::qd_read(file.path(spadesCBMdb, "data", "2011_pools.qs2")),
      by = "row_idx"))
  )

  out <- plotPoolProportions(pools)

  expect_is(out, "ggplot")
  expect_equal(subset(out$data, pool == "Soil"  )[order(year)]$proportion, c(0.7658, 0.7667),
               tolerance = 0.0001, scale = 1)
  expect_equal(subset(out$data, pool == "BGlive")[order(year)]$proportion, c(0.0426, 0.0426),
               tolerance = 0.0001, scale = 1)
  expect_equal(subset(out$data, pool == "AGlive")[order(year)]$proportion, c(0.1687, 0.1702),
               tolerance = 0.0001, scale = 1)
  expect_equal(subset(out$data, pool == "Snags" )[order(year)]$proportion, c(0.0229, 0.0205),
               tolerance = 0.0001, scale = 1)
})

test_that("mapTotalCarbon", {

  pools1985 <- merge(
    qs2::qd_read(file.path(spadesCBMdb, "data", "1985_key.qs2")),
    qs2::qd_read(file.path(spadesCBMdb, "data", "1985_pools.qs2")),
    by = "row_idx")

  pools1985 <- pools1985[, .(
      pixelIndex,
      totalCarbon = rowSums(pools1985[, .(
        Merch, Foliage, Other, CoarseRoots, FineRoots,
        AboveGroundVeryFastSoil, BelowGroundVeryFastSoil, AboveGroundFastSoil,
        BelowGroundFastSoil, MediumSoil, AboveGroundSlowSoil, BelowGroundSlowSoil,
        StemSnag, BranchSnag
      )])
    )][, lapply(.SD, sum), by = "pixelIndex"]

  rastTC <- terra::rast(masterRaster)
  terra::set.values(rastTC, pools1985$pixelIndex, pools1985$totalCarbon)

  out <- mapTotalCarbon(rastTC, year = 1985)
  expect_is(out, "ggplot")
  expect_match(out$labels$title, "Total Carbon in 1985", fixed = TRUE)
  expect_equal(
    terra::global(out$layers[[1]]$data$spatraster[[1]], "mean", na.rm = TRUE)[1,1],
    313.4, tolerance = 0.1, scale = 1)
})

test_that("mapNPP", {

  flux1985 <- merge(
    qs2::qd_read(file.path(spadesCBMdb, "data", "1985_key.qs2")),
    qs2::qd_read(file.path(spadesCBMdb, "data", "1985_flux.qs2")),
    by = "row_idx")

  flux1985 <- flux1985[, .(
    pixelIndex,
    NPP = rowSums(flux1985[, .(
      DeltaBiomass_AG, DeltaBiomass_BG,
      TurnoverMerchLitterInput, TurnoverFolLitterInput,
      TurnoverOthLitterInput, TurnoverCoarseLitterInput, TurnoverFineLitterInput
    )])
  )][, lapply(.SD, sum), by = "pixelIndex"]

  rastNPP <- terra::rast(masterRaster)
  terra::set.values(rastNPP, flux1985$pixelIndex, flux1985$NPP)

  out <- mapNPP(rastNPP, year = 1985)
  expect_is(out, "ggplot")
  expect_match(out$labels$title, "Net Primary Productivity (NPP) in 1985", fixed = TRUE)
  expect_match(out$labels$title, "5.804", fixed = TRUE)
  expect_equal(
    terra::global(out$layers[[1]]$data$spatraster[[1]], "mean", na.rm = TRUE)[1,1],
    5.8, tolerance = 0.1, scale = 1)
})

test_that("cTransfersAlluvial", {

  cTransfers <- qs2::qd_read(file.path(testDirs$testdata, "cTransfers_SPU-27-28_wildfire.qs2"))

  cTransfersPlot <- cTransfersAlluvial(cTransfers, distMatrixID = 371)

  expect_s3_class(cTransfersPlot, "ggplot")

  cTransfersPlot <- cTransfersAlluvial(cTransfers, distName = "wildfire", spuID = 28, sw =  TRUE, ask = FALSE)

  expect_s3_class(cTransfersPlot, "ggplot")
  expect_equal(unique(cTransfersPlot$data$disturbance_matrix_id), 371)

})

