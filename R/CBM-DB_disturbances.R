
utils::globalVariables(c(
  "locale_id", "name", "spatial_unit_id",
  "disturbance_type_id", "disturbance_matrix_id"
))

#' CBM-CFS3 Spatial Unit Disturbances Match
#'
#' Match disturbance names with CBM-CFS3 spatial unit disturbances.
#'
#' @param distTable \code{data.table} with columns 'spatial_unit_id' and 'name' (or 'distName').
#' The name column will be matched with disturbance names and descriptions
#' in the CBM-CFS3 database.
#' @param nearMatches logical. Allow for near matches; e.g. "clearcut" can match "clear-cut".
#' @param identical logical. Require identical matches.
#' @param ask logical.
#' If TRUE, prompt the user to choose the correct matches.
#' If FALSE, the function will look for a single match to each input.
#' @param listDist data.table. Optional. Result of a call to \code{\link{spuDistList}}.
#' A list of possible disturbances in the spatial unit(s) with columns
#' 'spatial_unit_id', 'disturbance_type_id', 'disturbance_matrix_id', 'name', 'description'.
#' @param ... arguments to \code{\link{spuDistList}}
#' for listing the possible disturbances in the spatial units.
#'
#' @return \code{data.table} with columns
#' 'spatial_unit_id', 'disturbance_type_id', 'disturbance_matrix_id', 'name', 'description'
#'
#' @export
#' @importFrom data.table data.table setkey
spuDistMatch <- function(distTable, nearMatches = TRUE, identical = !ask, ask = interactive(),
                         listDist = NULL, ...){

  # Check input
  if (!inherits(distTable, "data.table")){
    distTable <- tryCatch(
      data.table(distTable),
      error = function(e) stop(
        "'distTable' could not be converted to data.table: ", e$message, call. = FALSE))
  }

  reqCols <- c("spatial_unit_id", "name")
  if (!all(reqCols %in% names(distTable))) stop(
    "'distTable' must have the following columns: ",
    paste(shQuote(reqCols), collapse = ", "))

  # List disturbances
  if (is.null(listDist)){

    listDist <- spuDistList(spuIDs = distTable$spatial_unit_id, ...)

  }else{

    reqCols <- c("name", "description", "disturbance_type_id",
                 "spatial_unit_id", "disturbance_matrix_id")
    if (!all(reqCols %in% names(listDist))) stop(
      "listDist' must have the following columns: ",
      paste(shQuote(reqCols), collapse = ", "))
  }

  # Match disturbances
  matchTable <- unique(distTable[, .SD, .SDcols = !"spatial_unit_id"])

  matchTable$disturbance_type_id <- distMatch(
    distNames   = matchTable$name,
    inputTable  = matchTable,
    listDist    = listDist,
    nearMatches = nearMatches,
    identical   = identical,
    ask         = ask
  )$disturbance_type_id

  # Return table of disturbance matches
  spuDistTypes <- merge(
    distTable, matchTable,
    by = setdiff(names(matchTable), "disturbance_type_id"),
    all.x = TRUE, sort = FALSE)[
      , .(spatial_unit_id, disturbance_type_id)]

  distMatch <- merge(
    spuDistTypes, listDist,
    by = c("spatial_unit_id", "disturbance_type_id"),
    all.x = TRUE, sort = FALSE)

  setkey(distMatch, NULL)
  distMatch
}


#' CBM-CFS3 Spatial Unit Disturbances
#'
#' List the disturbances possible in spatial units.
#'
#' @param EXN logical. Use CBM-EXN CBM-CFS3 equivalent model data.
#' @param spuIDs Optional. Subset by spatial unit ID(s)
#' @param dbPath Path to CBM-CFS3 SQLite database file.
#' Required if EXN = TRUE or EXN = FALSE.
#' @param disturbance_matrix_association data.frame. Optional.
#' Alternative disturbance_matrix_association table with columns
#' "spatial_unit_id", "disturbance_type_id", and "disturbance_matrix_id".
#' Required if EXN = TRUE.
#' @param localeID CBM-CFS3 locale_id
#'
#' @return \code{data.table} with 'disturbance_type_tr' columns
#' 'disturbance_type_id', 'name', 'description'
#' and 'disturbance_matrix_association' columns
#' 'spatial_unit_id' and 'disturbance_matrix_id'
#'
#' @export
#' @importFrom data.table as.data.table
#' @importFrom RSQLite dbConnect dbDisconnect dbDriver dbListTables dbReadTable
spuDistList <- function(EXN = TRUE, spuIDs = NULL,
                        dbPath = NULL, disturbance_matrix_association = NULL,
                        localeID = 1){

  if (is.null(dbPath)) stop("'dbPath' input required")
  if (length(dbPath) != 1) stop("length(dbPath) must == 1")

  if (EXN){

    if (is.null(disturbance_matrix_association)) stop(
      "'disturbance_matrix_association' input required if EXN = TRUE")

   disturbance_matrix_association <- tryCatch(
      as.data.table(disturbance_matrix_association),
      error = function(e) stop(
        "disturbance_matrix_association failed to convert to data.table: ",
        e$message, call. = FALSE))

    reqCols <- c("spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id")
    if (!all(reqCols %in% names(disturbance_matrix_association))) stop(
      "'disturbance_matrix_association' must have the following columns: ",
      paste(shQuote(reqCols), collapse = ", "))
  }

  # Connect to database
  cbmDBcon <- dbConnect(dbDriver("SQLite"), dbname = dbPath)
  on.exit(dbDisconnect(cbmDBcon))

  # Read database tables
  ## Read more about the 6 tables related to disturbance matrices here:
  ## https://docs.google.com/spreadsheets/d/1TFBQiRH4z54l8ROX1N02OOiCHXMe20GSaExiuUqsC0Q
  cbmTableNames <- c("disturbance_type_tr", if (!EXN) "disturbance_matrix_association")

  cbmDB <- list()
  for (cbmTableName in cbmTableNames) {
    cbmDB[[cbmTableName]] <- dbReadTable(cbmDBcon, cbmTableName) |> as.data.table()
  }
  if (EXN){
    cbmDB[["disturbance_matrix_association"]] <- disturbance_matrix_association
  }

  # Merge and return
  spuDistList <- cbmDB[["disturbance_matrix_association"]][
    subset(cbmDB[["disturbance_type_tr"]], locale_id == localeID),
    on = .(disturbance_type_id = disturbance_type_id), nomatch = NULL]

  if (!is.null(spuIDs)) spuDistList <- subset(spuDistList, spatial_unit_id %in% spuIDs)

  return(
    spuDistList[, intersect(
      c("spatial_unit_id", "disturbance_type_id", "sw_hw", "disturbance_matrix_id", "name", "description"),
      names(spuDistList)),
      with = FALSE]
  )
}


#' CBM-CFS3 Historical Disturbances
#'
#' Identifies the stand-replacing wildfire disturbance in each spatial unit.
#'
#' In all spatial units in Canada, the historical disturbance is set to fire.
#' Historical disturbances in CBM-CFS3 are used for "filling-up" the soil-related carbon pools.
#' Boudewyn et al. (2007) translate the m3/ha curves into biomass per ha in each of four pools:
#' total biomass for stem wood, total biomass for bark, total biomass for branches and total
#' biomass for foliage.
#' Biomass in coarse and fine roots, in aboveground- and belowground- very-fast, -fast, -slow,
#' in medium-soil, and in snags still needs to be estimated.
#' A stand-replacing fire disturbance is used in a disturb-grow cycle, where stands are disturbed
#' and regrown with turnover, overmature, decay, functioning until the dead organic matter pools
#' biomass values stabilize (+/- 10%) (TODO: check this).
#'
#' @param spuIDs Spatial unit ID(s)
#' @param localeID CBM-CFS3 locale_id
#' @param ask logical.
#' If TRUE, prompt the user to choose the correct disturbance matches.
#' If FALSE, the function will look for exact name matches.
#' @param ... arguments to \code{\link{spuDistMatch}}
#'
#' @export
spuHistDist <- function(spuIDs, localeID = 1, ask = FALSE, ...) {

  if (length(spuIDs) < 1) stop("length(spuIDs) must be >= 1")

  # Set disturbance name matches
  histDistName <- list(`1` = "Wildfire")
  if (!as.character(localeID) %in% names(histDistName)) stop(
    "CBMutils::histDist does not support finding historical disturbances for locale_id ",
    localeID, " (yet).")

  # Return matching records
  spuDistMatch(
    data.frame(spatial_unit_id = spuIDs, name = histDistName[[as.character(localeID)]]),
    localeID = localeID, ask = FALSE, ...)
}


#' See disturbances
#'
#' Retrieve disturbance source pools, sink pools, and the proportions transferred.
#'
#' @param EXN logical. Use CBM-EXN CBM-CFS3 equivalent model data.
#' @param matrixIDs character. Optional. Subset disturbances by disturbance_matrix_id
#' @param dbPath Path to CBM-CFS3 SQLite database file.
#' Required if EXN = FALSE
#' @param disturbance_matrix_value disturbance_matrix_value table from CBM-EXN
#' Required if EXN = TRUE
#'
#' @return List of `data.frame` named by disturbance_matrix_id
#'
#' @export
#' @importFrom data.table as.data.table
#' @importFrom RSQLite dbConnect dbDisconnect dbDriver dbReadTable
seeDist <- function(EXN = TRUE, matrixIDs = NULL,
                    dbPath = NULL, disturbance_matrix_value = NULL){

  if (EXN){

    if (is.null(disturbance_matrix_value)) stop(
      "'disturbance_matrix_value' input required if EXN = TRUE")

    disturbance_matrix_value <- tryCatch(
      as.data.table(disturbance_matrix_value),
      error = function(e) stop(
        "disturbance_matrix_value failed to convert to data.table: ",
        e$message, call. = FALSE))

    reqCols <- c("disturbance_matrix_id", "source_pool", "sink_pool", "proportion")
    if (!all(reqCols %in% names(disturbance_matrix_value))) stop(
      "'disturbance_matrix_value' must have the following columns: ",
      paste(shQuote(reqCols), collapse = ", "))

  }else{

    if (is.null(dbPath)) stop(
      "'dbPath' input required if EXN = FALSE")
    if (length(dbPath) != 1) stop("length(dbPath) must == 1")

    # Connect to database
    cbmDBcon <- dbConnect(dbDriver("SQLite"), dbname = dbPath)
    on.exit(dbDisconnect(cbmDBcon))

    cbmDBM <- {
      tableNames <- c("disturbance_matrix_value", "pool")
      names(tableNames) <- tableNames
      lapply(tableNames, function(nm){
        as.data.table(dbReadTable(cbmDBcon, nm))
      })
    }

    # CRAN requirement: predefine variables
    id <- source_pool_id <- source_pool <- sink_pool_id <- sink_pool <- NULL
    disturbance_matrix_id <- proportion <- code <- NULL

    cbmDBM[["pool_source"]] <- copy(cbmDBM[["pool"]])[, source_pool := code]
    cbmDBM[["pool_sink"]]   <- copy(cbmDBM[["pool"]])[, sink_pool   := code]
    disturbance_matrix_value <- cbmDBM[["disturbance_matrix_value"]] |>
      merge(cbmDBM[["pool_source"]][, .(id, source_pool)], by.x = "source_pool_id", by.y = "id") |>
      merge(cbmDBM[["pool_sink"  ]][, .(id, sink_pool  )], by.x = "sink_pool_id",   by.y = "id")
    disturbance_matrix_value <- disturbance_matrix_value[
      , .(disturbance_matrix_id, source_pool_id, source_pool, sink_pool_id, sink_pool, proportion)]
  }

  distTables <- split(disturbance_matrix_value, disturbance_matrix_value$disturbance_matrix_id)
  if (!is.null(matrixIDs)) distTables <- distTables[as.character(matrixIDs)]
  return(distTables)
}

#' See disturbances in simList
#'
#' get the descriptive name and proportions transferred for disturbances in a simulation
#' requires a simulation list post simulations (from spades())
#' and returns a list of data.frames. Each data had the descriptive name of a
#' disturbance used in the simulations, the disturbance matrix identification
#' number from cbm_defaults, the pool from which carbon is taken (source pools)
#' in this specific disturbance, the pools into which carbon goes, and the
#' proportion in which the carbon-transfers are completed.
#'
#' @template simCBM
#'
#' @return List of `data.frame` for each disturbance matrix id in the study area, named by disturbance name
#'
#' @export
simDist <- function(simCBM) {

  # Getting the disturbances in study area
  DMID <- unique(simCBM@.envir$disturbanceMeta[, 6])

  # Getting all disturbance tables from seeDist
  allDist <- seeDist(EXN = FALSE, dbPath = simCBM@.envir$dbPath)
  # Subsetting table list to only those relevant to study area
  subsetDist <- allDist[names(allDist) %in% DMID$disturbance_matrix_id]

  # each data.frame gets a descriptive name
  names(subsetDist) <- unique(simCBM@.envir$disturbanceMatrix[DMID, on = "disturbance_matrix_id", .(disturbance_matrix_id, name)])$name
  # description
  # "Salvage uprooting and burn for Boreal Plains"
  return(subsetDist)
}


