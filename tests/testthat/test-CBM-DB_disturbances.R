
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Download CBM-CFS3 databases
dbPath <- {
  url = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db"
  destfile <- file.path(testDirs$temp$inputs, basename(url))
  if (!file.exists(destfile)) download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
  destfile
}
exnDB <- {
  csvNames <- c("disturbance_matrix_association", "disturbance_matrix_value")
  lapply(setNames(csvNames, csvNames), function(csvName){
    url = file.path(
      "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_exn",
      paste0(csvName, ".csv"))
    destfile <- file.path(testDirs$temp$inputs, basename(url))
    if (!file.exists(destfile)) download.file(url = url, destfile = destfile, quiet = TRUE)
    read.csv(destfile, stringsAsFactors = FALSE, row.names = NULL)
  })
}

test_that("distList", {

  listDist <- distList(dbPath = dbPath)

  expect_true(inherits(listDist, "data.table"))
  expect_true(all(c(
    "disturbance_type_id", "name", "description"
  ) %in% names(listDist)))

  expect_true(nrow(listDist) == 172)
})

test_that("distMatch", {

  distNames <- c("Wildfire", "Clearcut harvesting without salvage",
                 "Generic 20% mortality", "Deforestation")

  listDist <- distMatch(distNames, dbPath = dbPath, ask = FALSE)

  expect_true(inherits(listDist, "data.table"))
  expect_true(all(c(
    "disturbance_type_id", "name", "description"
  ) %in% names(listDist)))

  expect_true(nrow(listDist) == 4)

  expect_equal(listDist$disturbance_type_id, c(1, 204, 168, 7))

})

test_that("spuDistList", {

  # cbm_exn = FALSE

  ## Expect error: dbPath missing
  expect_error(
    spuDistList(spuIDs = 27, EXN = FALSE, dbPath = NULL,
            disturbance_matrix_association = exnDB[["disturbance_matrix_association"]]),
    "dbPath"
  )

  ## Expect success
  listDist <- spuDistList(spuIDs = 27, EXN = FALSE, dbPath = dbPath)

  expect_true(inherits(listDist, "data.table"))

  expect_true(all(c(
    "spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_true(all(listDist$spatial_unit_id == 27))
  expect_true(nrow(listDist) == 133)


  # EXN = TRUE

  ## Expect error: dbPath missing
  expect_error(
    spuDistList(spuIDs = 27, EXN = TRUE, dbPath = NULL,
            disturbance_matrix_association = exnDB[["disturbance_matrix_association"]]),
    'dbPath'
  )

  ## Expect error: disturbance_matrix_association missing
  expect_error(
    spuDistList(spuIDs = 27, EXN = TRUE, dbPath = dbPath,
            disturbance_matrix_association = NULL),
    'disturbance_matrix_association'
  )

  ## Expect success
  listDist <- spuDistList(
    spuIDs = 27, EXN = TRUE, dbPath = dbPath,
    disturbance_matrix_association = exnDB[["disturbance_matrix_association"]])
  expect_true(all(listDist$spatial_unit_id == 27))
  expect_true(nrow(listDist) == 266)

})

test_that("spuDistMatch", {

  # Set list of disturbance types
  distTypes <- rbind(
    data.frame(eventID = 1, wholeStand = 1, name = "Wildfire"),
    data.frame(eventID = 2, wholeStand = 1, name = "Clearcut harvesting without salvage"),
    data.frame(eventID = 3, wholeStand = 0, name = "Generic 20% mortality"),
    data.frame(eventID = 4, wholeStand = 1, name = "Deforestation")
  )

  # EXN = FALSE

  ## Expect error: dbPath missing
  expect_error(
    spuDistMatch(distTable = cbind(spatial_unit_id = 28, distTypes),
                 EXN = FALSE, ask = FALSE, dbPath = NULL),
    "dbPath"
  )

  ## Try with a single spuID
  listDist <- spuDistMatch(
    distTable = cbind(spatial_unit_id = 28, distTypes),
    EXN = FALSE, ask = FALSE, dbPath = dbPath)

  expect_true(inherits(listDist, "data.table"))

  expect_true(all(c(
    "disturbance_type_id", "spatial_unit_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_equal(listDist$disturbance_matrix_id, c(371, 160, 91, 26))

  ## Try with 2 spuIDs
  listDist <- spuDistMatch(
    distTable = rbind(cbind(spatial_unit_id = 27, distTypes),
                      cbind(spatial_unit_id = 28, distTypes)),
    EXN = FALSE, ask = FALSE, dbPath = dbPath)

  expect_true(inherits(listDist, "data.table"))
  expect_true(all(c(
    "disturbance_type_id", "spatial_unit_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_equal(listDist$disturbance_matrix_id, c(
    378, 160, 91, 26,
    371, 160, 91, 26
  ))

  ## Expect error: name does not have an exact match and ask = FALSE
  expect_error(
    spuDistMatch(
      distTable = data.frame(spatial_unit_id = 27, name = "clearcut"),
      EXN = FALSE, ask = FALSE, dbPath = dbPath)
  )

  ## Test with listDist provided
  listDist2 <- spuDistMatch(
    distTable = rbind(cbind(spatial_unit_id = 27, distTypes),
                      cbind(spatial_unit_id = 28, distTypes)),
    listDist = spuDistList(EXN = FALSE, dbPath = dbPath),
    ask = FALSE)

  expect_identical(listDist, listDist2)


  # EXN = TRUE

  ## Expect error: dbPath missing
  expect_error(
    spuDistMatch(
      distTable = cbind(spatial_unit_id = 28, distTypes),
      EXN = TRUE, ask = FALSE, dbPath = NULL,
      disturbance_matrix_association = exnDB[["disturbance_matrix_association"]]),
    "dbPath"
  )

  ## Expect error: disturbance_matrix_association missing
  expect_error(
    spuDistMatch(
      distTable = cbind(spatial_unit_id = 28, distTypes),
      EXN = TRUE, ask = FALSE, dbPath = dbPath,
      disturbance_matrix_association = NULL),
    "disturbance_matrix_association"
  )

  ## Test 2 spuIDs
  listDist <- spuDistMatch(
    distTable = rbind(cbind(spatial_unit_id = 27, distTypes),
                      cbind(spatial_unit_id = 28, distTypes)),
    EXN = TRUE, ask = FALSE, dbPath = dbPath,
    disturbance_matrix_association = exnDB[["disturbance_matrix_association"]])

  expect_true(inherits(listDist, "data.table"))
  expect_true(all(c(
    "disturbance_type_id", "spatial_unit_id", "sw_hw", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_equal(listDist$disturbance_matrix_id, c(
    378, 858, 160, 640, 91, 571, 26, 506,
    371, 851, 160, 640, 91, 571, 26, 506
  ))

  ## Expect error: name does not have an exact match and ask = FALSE
  expect_error(
    spuDistMatch(
      distTable = data.frame(spatial_unit_id = 27, name = "clearcut"),
      EXN = TRUE, ask = FALSE, dbPath = dbPath,
      disturbance_matrix_association = exnDB[["disturbance_matrix_association"]])
  )
})

test_that("spuHistDist", {

  # EXN = FALSE

  ## Expect error: dbPath missing
  expect_error(
    spuHistDist(spuIDs = 27, EXN = FALSE, dbPath = NULL),
    "dbPath"
  )

  listDist <- spuHistDist(spuIDs = 27, EXN = FALSE, dbPath = dbPath)

  expect_true(inherits(listDist, "data.table"))

  expect_true(all(c(
    "spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_true(all(listDist$spatial_unit_id == 27))

  expect_true(nrow(listDist) == 1)

  # Result should be for name "Wildfire"
  expect_true(listDist$disturbance_matrix_id == 378)


  # EXN = TRUE

  ## Expect error: dbPath missing
  expect_error(
    spuHistDist(spuIDs = 27, EXN = TRUE, dbPath = NULL),
    "dbPath"
  )

  ## Expect error: disturbance_matrix_association missing
  expect_error(
    spuHistDist(spuIDs = 27, EXN = TRUE, dbPath = dbPath),
    "disturbance_matrix_association"
  )

  listDist <- spuHistDist(
    spuIDs = 27, EXN = TRUE, dbPath = dbPath,
    disturbance_matrix_association = exnDB[["disturbance_matrix_association"]])

  expect_true(inherits(listDist, "data.table"))

  expect_true(all(c(
    "spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_true(all(listDist$spatial_unit_id == 27))

  expect_true(nrow(listDist) == 2)

  # Result should be for name "Wildfire"
  expect_true(all(listDist$disturbance_matrix_id == c(378, 858)))

})

test_that("seeDist", {

  # EXN = FALSE

  ## Expect error: dbPath missing
  expect_error(
    seeDist(EXN = FALSE, dbPath = NULL),
    "dbPath"
  )

  distVals <- seeDist(EXN = FALSE, dbPath = dbPath)

  expect_true(inherits(distVals, "list"))
  expect_true(inherits(distVals[[1]], "data.table"))

  expect_true(all(c(
    "disturbance_matrix_id", "source_pool", "sink_pool", "proportion"
  ) %in% names(distVals[[1]])))

  ## Test providing a matrix ID
  distVals <- seeDist(matrixIDs = 2, EXN = FALSE, dbPath = dbPath)

  expect_true(inherits(distVals, "list"))
  expect_true(inherits(distVals[[1]], "data.table"))

  expect_true(all(c(
    "disturbance_matrix_id", "source_pool", "sink_pool", "proportion"
  ) %in% names(distVals[[1]])))

  expect_identical(names(distVals), "2")
  expect_true(all(distVals[["2"]]$disturbance_matrix_id == 2))


  # EXN = TRUE

  ## Expect error: disturbance_matrix_value missing
  expect_error(
    seeDist(EXN = TRUE, dbPath = NULL),
    "disturbance_matrix_value"
  )

  distVals <- seeDist(EXN = TRUE, disturbance_matrix_value = exnDB[["disturbance_matrix_value"]])

  expect_true(inherits(distVals, "list"))
  expect_true(inherits(distVals[[1]], "data.table"))

  expect_true(all(c(
    "disturbance_matrix_id", "source_pool", "sink_pool", "proportion"
  ) %in% names(distVals[[1]])))

  ## Test providing a matrix ID
  distVals <- seeDist(matrixIDs = 2, EXN = TRUE, disturbance_matrix_value = exnDB[["disturbance_matrix_value"]])

  expect_true(inherits(distVals, "list"))
  expect_true(inherits(distVals[[1]], "data.table"))

  expect_true(all(c(
    "disturbance_matrix_id", "source_pool", "sink_pool", "proportion"
  ) %in% names(distVals[[1]])))

  expect_identical(names(distVals), "2")
  expect_true(all(distVals[["2"]]$disturbance_matrix_id == 2))

})

test_that("simDist", {

})


