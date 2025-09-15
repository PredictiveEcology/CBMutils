
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

emissionsProducts <- qs::qread(file.path(testDirs$testdata, "CBM_core_outputs/SK/emissionsProducts.qs"))

spadesCBMdb <- file.path(testDirs$temp$inputs, "CBM_core_outputs_SK", "CBM_core_db")
if (!file.exists(spadesCBMdb)){
  dir.create(dirname(spadesCBMdb))
  file.copy(file.path(testDirs$testdata, "CBM_core_outputs/SK/CBM_core_db"), dirname(spadesCBMdb), recursive = TRUE)
}

masterRaster <- terra::rast(
  vals = 1, crs = "local",
  xmin = 0, ymin = 0,
  ncols = 1950, xmax = 1950 * 30,
  nrows = 1900, ymax = 1900 * 30)

simCBM <- list(spadesCBMdb = spadesCBMdb, masterRaster = masterRaster, emissionsProducts = emissionsProducts)


test_that("plotEmissionsProducts", {

  out <- plotEmissionsProducts(emissionsProducts)
  expect_is(out, "ggplot")
})

test_that("simPlotEmissionsProducts", {

  out <- simPlotEmissionsProducts(simCBM)
  expect_is(out, "ggplot")
})

test_that("plotPoolProportions", {

  pools <- rbind(
    cbind(year = 1985, merge(
      qs::qread(file.path(spadesCBMdb, "data", "1985_key.qs")),
      qs::qread(file.path(spadesCBMdb, "data", "1985_pools.qs")),
      by = "row_idx")),
    cbind(year = 2011, merge(
      qs::qread(file.path(spadesCBMdb, "data", "2011_key.qs")),
      qs::qread(file.path(spadesCBMdb, "data", "2011_pools.qs")),
      by = "row_idx"))
  )

  out <- plotPoolProportions(pools)

  expect_is(out, "ggplot")
  expect_equal(subset(out$data, pool == "soil"  )[order(year)]$proportion, c(0.7658, 0.7667),
               tolerance = 0.0001, scale = 1)
  expect_equal(subset(out$data, pool == "BGlive")[order(year)]$proportion, c(0.0426, 0.0426),
               tolerance = 0.0001, scale = 1)
  expect_equal(subset(out$data, pool == "AGlive")[order(year)]$proportion, c(0.1687, 0.1702),
               tolerance = 0.0001, scale = 1)
  expect_equal(subset(out$data, pool == "snags" )[order(year)]$proportion, c(0.0229, 0.0205),
               tolerance = 0.0001, scale = 1)
})

test_that("simPlotPoolProportions", {

  out <- simPlotPoolProportions(
    simCBM = simCBM, years = c(1985, 2011)
  )

  expect_is(out, "ggplot")
  expect_equal(subset(out$data, pool == "soil"  )[order(year)]$proportion, c(0.7658, 0.7667),
               tolerance = 0.0001, scale = 1)
  expect_equal(subset(out$data, pool == "BGlive")[order(year)]$proportion, c(0.0426, 0.0426),
               tolerance = 0.0001, scale = 1)
  expect_equal(subset(out$data, pool == "AGlive")[order(year)]$proportion, c(0.1687, 0.1702),
               tolerance = 0.0001, scale = 1)
  expect_equal(subset(out$data, pool == "snags" )[order(year)]$proportion, c(0.0229, 0.0205),
               tolerance = 0.0001, scale = 1)
})

test_that("mapTotalCarbon", {

  pools1985 <- merge(
    qs::qread(file.path(spadesCBMdb, "data", "1985_key.qs")),
    qs::qread(file.path(spadesCBMdb, "data", "1985_pools.qs")),
    by = "row_idx")

  out <- mapTotalCarbon(pools = pools1985, masterRaster = masterRaster, year = 1985)
  expect_is(out, "ggplot")
  expect_match(out$labels$title, "Total Carbon in 1985", fixed = TRUE)
  expect_equal(
    terra::global(out$layers[[1]]$data$spatraster[[1]], "mean", na.rm = TRUE)[1,1],
    313.4, tolerance = 0.1, scale = 1)
})

test_that("simMapTotalCarbon", {

  out <- simMapTotalCarbon(simCBM, year = 1985, useCache = FALSE)
  expect_is(out, "ggplot")
  expect_match(out$labels$title, "Total Carbon in 1985", fixed = TRUE)
  expect_equal(
    terra::global(out$layers[[1]]$data$spatraster[[1]], "mean", na.rm = TRUE)[1,1],
    313.4, tolerance = 0.1, scale = 1)
})

test_that("mapNPP", {

  flux1985 <- merge(
    qs::qread(file.path(spadesCBMdb, "data", "1985_key.qs")),
    qs::qread(file.path(spadesCBMdb, "data", "1985_flux.qs")),
    by = "row_idx")

  out <- mapNPP(flux = flux1985, masterRaster = masterRaster, year = 1985)
  expect_is(out, "ggplot")
  expect_match(out$labels$title, "Net Primary Productivity (NPP) in 1985", fixed = TRUE)
  expect_match(out$labels$title, "5.804", fixed = TRUE)
  expect_equal(
    terra::global(out$layers[[1]]$data$spatraster[[1]], "mean", na.rm = TRUE)[1,1],
    5.8, tolerance = 0.1, scale = 1)
})

test_that("simMapNPP", {

  out <- simMapNPP(simCBM, year = 1985, useCache = FALSE)
  expect_is(out, "ggplot")
  expect_match(out$labels$title, "Net Primary Productivity (NPP) in 1985", fixed = TRUE)
  expect_match(out$labels$title, "5.804", fixed = TRUE)
  expect_equal(
    terra::global(out$layers[[1]]$data$spatraster[[1]], "mean", na.rm = TRUE)[1,1],
    5.8, tolerance = 0.1, scale = 1)
})



