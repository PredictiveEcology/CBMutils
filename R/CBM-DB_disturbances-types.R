
#' CBM-CFS3 Disturbances Match
#'
#' Match disturbance names with CBM-CFS3 disturbance types.
#'
#' @param distNames character.
#' Disturbance names to match with CBM-CFS3 database disturbance type names.
#' @param nearMatches logical. Allow for near matches; e.g. "clearcut" can match "clear-cut".
#' @param identical logical. Require identical matches.
#' @param ask logical.
#' If TRUE, prompt the user to choose the correct matches.
#' If FALSE, the function will look for a single match to each input.
#' @param listDist data.table. Optional. Result of a call to \code{\link{distList}}.
#' Table of disturbance types with columns
#' 'disturbance_type_id', 'name', 'description'.
#' @param dbPath Path to CBM-CFS3 SQLite database file. Required if `listDist` is NULL.
#' @param localeID CBM-CFS3 locale_id
#' @param ... additional arguments to \code{\link{.matchSelect}}
#'
#' @return \code{data.table} with columns 'disturbance_type_id', 'name', 'description'
#'
#' @export
#' @importFrom data.table copy data.table setkey
distMatch <- function(distNames, nearMatches = TRUE, identical = !ask, ask = interactive(),
                      listDist = NULL, dbPath = NULL, localeID = 1, ...){

  # Set near matches
  if (nearMatches) nearMatches <- list(
    `clearcut` = c("clear cut", "clear-cut"),
    `wildfire` = c("wild fire", "wild-fire")
  )

  # List disturbances
  if (is.null(listDist)){

    listDist <- distList(dbPath = dbPath, localeID = 1)

  }else{

    reqCols <- c("disturbance_type_id", "name", "description")
    if (!all(reqCols %in% names(listDist))) stop(
      "listDist' must have the following columns: ",
      paste(shQuote(reqCols), collapse = ", "))

    listDist <- unique(listDist[, .(disturbance_type_id, name, description)])
  }

  # Match disturbances
  matchIdx <- .matchSelect(
    inputs      = distNames,
    choices     = listDist$name,
    choiceTable = listDist[, .(disturbance_type_id, name)],
    choiceTableExtra = listDist[, .(description)],
    identical   = identical,
    nearMatches = nearMatches,
    ask         = ask,
    ...
  )

  # Return table of disturbance matches
  matchTable <- listDist[matchIdx,]
  setkey(matchTable, NULL)
  matchTable
}


#' CBM-CFS3 Disturbance types
#'
#' List CBM-CFS3 disturbance types.
#'
#' @param dbPath Path to CBM-CFS3 SQLite database file.
#' @param localeID CBM-CFS3 locale_id
#'
#' @return \code{data.table} with 'disturbance_type_tr' columns
#' 'disturbance_type_id', 'name', 'description'
#'
#' @importFrom RSQLite dbConnect dbDisconnect dbDriver dbListTables dbReadTable
#' @importFrom data.table as.data.table setkey
distList <- function(dbPath, localeID = 1){

  if (is.null(dbPath)) stop("'dbPath' input required")
  if (length(dbPath) != 1) stop("length(dbPath) must == 1")

  # Connect to database
  cbmDBcon <- dbConnect(dbDriver("SQLite"), dbname = dbPath)
  on.exit(dbDisconnect(cbmDBcon))

  # Read and return disturbance types
  distList <- as.data.table(dbReadTable(cbmDBcon, "disturbance_type_tr"))

  distList <- subset(distList, locale_id == localeID)
  distList[, locale_id := NULL]
  distList[, id := NULL]

  setkey(distList, disturbance_type_id)

  distList
}


