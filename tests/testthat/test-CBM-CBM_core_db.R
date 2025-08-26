
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

spadesCBMdb <- file.path(testDirs$temp$inputs, "CBM_core_outputs_SK", "CBM_core_db")
if (!file.exists(spadesCBMdb)){
  dir.create(dirname(spadesCBMdb))
  file.copy(file.path(testDirs$testdata, "CBM_core_outputs/SK/CBM_core_db"), dirname(spadesCBMdb), recursive = TRUE)
}

test_that("spadesCBMdbWrite", {

  spadesCBMdbTemp <- file.path(testDirs$temp$outputs, "spadesCBMdb", "spadesCBMdbWrite")

  spadesCBMdbWrite(
    spadesCBMdbTemp,
    cbm_vars   = list(key = "test", flux = "test", pools = "test"),
    year       = 1985,
    parameters = FALSE,
    state      = FALSE,
    flux       = TRUE,
    pools      = FALSE
  )

  expect_true(file.exists(.spadesCBMdbDataPath(spadesCBMdbTemp, 1985, "key")))
  expect_true(file.exists(.spadesCBMdbDataPath(spadesCBMdbTemp, 1985, "flux")))
  expect_false(file.exists(.spadesCBMdbDataPath(spadesCBMdbTemp, 1985, "pools")))
})

test_that("simCBMdbWrite", {

  spadesCBMdbTemp <- file.path(testDirs$temp$outputs, "spadesCBMdb", "simCBMdbWrite")

  simCBM <- SpaDES.core::simInit(times = list(start = 1985, end = 2011))
  simCBM$spadesCBMdb <- spadesCBMdbTemp
  simCBM$cbm_vars <- list(key = "test", flux = "test", pools = "test")

  simCBMdbWrite(
    simCBM,
    parameters = FALSE,
    state      = FALSE,
    flux       = TRUE,
    pools      = FALSE
  )

  expect_true(file.exists(.spadesCBMdbDataPath(spadesCBMdbTemp, 1985, "key")))
  expect_true(file.exists(.spadesCBMdbDataPath(spadesCBMdbTemp, 1985, "flux")))
  expect_false(file.exists(.spadesCBMdbDataPath(spadesCBMdbTemp, 1985, "pools")))
})

test_that("spadesCBMdbReadRaw", {

  key2011 <- spadesCBMdbReadRaw(
    spadesCBMdb,
    year  = 2011,
    table = "key"
  )
  expect_is(key2011, "data.table")
  expect_identical(data.table::key(key2011), "cohortID")
  expect_true("pixelIndex" %in% names(key2011))
  expect_true("row_idx"    %in% names(key2011))
  expect_equal(nrow(key2011), 1347529)

  pools1985 <- spadesCBMdbReadRaw(
    spadesCBMdb,
    year  = 1985,
    table = "pools"
  )
  expect_is(pools1985, "data.table")
  expect_identical(data.table::key(pools1985), "row_idx")
  expect_equal(ncol(pools1985), 21)
  expect_equal(nrow(pools1985), 768)

  # Expect error: table not found
  expect_error(
    spadesCBMdbReadRaw(
      spadesCBMdb,
      year  = 1986,
      table = "pools"
    ))
})

test_that("simCBMdbReadRaw", {

  simCBM <- SpaDES.core::simInit(times = list(start = 1985, end = 2011))
  simCBM$spadesCBMdb <- spadesCBMdb

  key2011 <- simCBMdbReadRaw(
    simCBM = simCBM,
    year   = 2011,
    table  = "key"
  )
  expect_is(key2011, "data.table")
  expect_identical(data.table::key(key2011), "cohortID")
  expect_true("pixelIndex" %in% names(key2011))
  expect_true("row_idx"    %in% names(key2011))
  expect_equal(nrow(key2011), 1347529)

  pools1985 <- simCBMdbReadRaw(
    simCBM = simCBM,
    table  = "pools"
  )
  expect_is(pools1985, "data.table")
  expect_identical(data.table::key(pools1985), "row_idx")
  expect_equal(ncol(pools1985), 21)
  expect_equal(nrow(pools1985), 768)

  # Expect error: table not found
  expect_error(
    simCBMdbReadRaw(
      simCBM = simCBM,
      year   = 1986,
      table  = "pools"
    ))
})

test_that("spadesCBMdbReadTable", {

  pools1985 <- spadesCBMdbReadTable(
    spadesCBMdb,
    year  = 1985,
    table = "pools",
    useCache = FALSE
  )
  expect_is(pools1985, "data.table")
  expect_identical(data.table::key(pools1985), "cohortID")
  expect_true("pixelIndex" %in% names(pools1985))
  expect_true("row_idx"    %in% names(pools1985))
  expect_equal(ncol(pools1985), 23)
  expect_equal(nrow(pools1985), 1347529)

  expect_equal(
    spadesCBMdbReadTable(
      spadesCBMdb,
      year  = 1985,
      table = "pools",
      useCache = TRUE
    ),
    pools1985,
    check.attributes = FALSE)

  # Expect error: table not found
  expect_error(
    spadesCBMdbReadTable(
      spadesCBMdb,
      year  = 1986,
      table = "pools"
    ))
})

test_that("simCBMdbReadTable", {

  simCBM <- SpaDES.core::simInit(times = list(start = 1985, end = 2011))
  simCBM$spadesCBMdb <- spadesCBMdb

  pools1985 <- simCBMdbReadTable(
    simCBM   = simCBM,
    table    = "pools",
    useCache = FALSE
  )
  expect_is(pools1985, "data.table")
  expect_identical(data.table::key(pools1985), "cohortID")
  expect_true("pixelIndex" %in% names(pools1985))
  expect_true("row_idx"    %in% names(pools1985))
  expect_equal(ncol(pools1985), 23)
  expect_equal(nrow(pools1985), 1347529)

  expect_equal(
    simCBMdbReadTable(
      simCBM   = simCBM,
      table    = "pools",
      useCache = TRUE
    ),
    pools1985,
    check.attributes = FALSE)

  # Expect error: table not found
  expect_error(
    simCBMdbReadTable(
      simCBM = simCBM,
      year   = 1986,
      table  = "pools"
    ))
})

test_that("spadesCBMdbReadSummary", {

  pools1985 <- spadesCBMdbReadSummary(
    spadesCBMdb,
    year     = 1985,
    summary  = "pools",
    by       = "pixelIndex",
    useCache = FALSE
  )
  expect_is(pools1985, "data.table")
  expect_identical(data.table::key(pools1985), "pixelIndex")
  expect_false("cohortID" %in% names(pools1985))
  expect_false("row_idx"    %in% names(pools1985))
  expect_equal(nrow(pools1985), 1347529)

  expect_equal(
    spadesCBMdbReadSummary(
      spadesCBMdb,
      year     = 1985,
      summary  = "pools",
      by       = "pixelIndex",
      useCache = TRUE
    ),
    pools1985,
    check.attributes = FALSE)

  poolsByYear <- spadesCBMdbReadSummary(
    spadesCBMdb,
    years    = c(1985, 2011),
    summary  = "pools",
    by       = "year",
    useCache = FALSE
  )
  expect_is(poolsByYear, "data.table")
  expect_identical(data.table::key(poolsByYear), "year")
  expect_equal(poolsByYear$year,  c(1985, 2011))
  expect_equal(poolsByYear$Merch, c(44240871, 48673534))
})

test_that("simCBMdbReadSummary", {

  simCBM <- SpaDES.core::simInit(times = list(start = 1985, end = 2011))
  simCBM$spadesCBMdb <- spadesCBMdb

  pools1985 <- simCBMdbReadSummary(
    simCBM   = simCBM,
    year     = 1985,
    summary  = "pools",
    by       = "pixelIndex",
    useCache = FALSE
  )
  expect_is(pools1985, "data.table")
  expect_identical(data.table::key(pools1985), "pixelIndex")
  expect_false("cohortID" %in% names(pools1985))
  expect_false("row_idx"    %in% names(pools1985))
  expect_equal(nrow(pools1985), 1347529)

  expect_equal(
    simCBMdbReadSummary(
      simCBM   = simCBM,
      year     = 1985,
      summary  = "pools",
      by       = "pixelIndex",
      useCache = TRUE
    ),
    pools1985,
    check.attributes = FALSE)

  poolsByYear <- simCBMdbReadSummary(
    simCBM   = simCBM,
    years    = c(1985, 2011),
    summary  = "pools",
    by       = "year",
    useCache = FALSE
  )
  expect_is(poolsByYear, "data.table")
  expect_identical(data.table::key(poolsByYear), "year")
  expect_equal(poolsByYear$year,  c(1985, 2011))
  expect_equal(poolsByYear$Merch, c(44240871, 48673534))
})



