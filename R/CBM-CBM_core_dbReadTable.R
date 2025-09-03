utils::globalVariables(c("cohortID", "pixelIndex", "row_idx"))

#' simList: Read SpaDES CBM database table
#'
#' Read a table of cohort data from a simulation's **CBM_core** SpaDES CBM database.
#'
#' @template simCBM
#' @param year numeric. Year of simulation results.
#' Defaults to the current simulation year.
#' @inheritParams spadesCBMdbReadTable
#' @inherit spadesCBMdbReadTable return
#' @export
simCBMdbReadTable <- function(simCBM, year, table, useCache = TRUE){

  if (missing(year)){
    year <- SpaDES.core::convertTimeunit(SpaDES.core::times(simCBM)$current, "year")
  }

  spadesCBMdbReadTable(simCBM$spadesCBMdb, year = year, table = table, useCache = useCache)
}

#' Read SpaDES CBM database table
#'
#' Read a table of cohort data from a **CBM_core** SpaDES CBM database.
#'
#' @template spadesCBMdb
#' @param year numeric. Year of simulation results.
#' @param table character. Name of database table to read.
#' One of 'key', 'parameters', 'state', 'flux', or 'pools'.
#' @param useCache logical. Cache database query result to the database's
#' `cache` directory with \code{\link[reproducible]{Cache}}.
#'
#' @return data.table with key 'cohortID',
#' columns 'pixelIndex' and 'row_idx' (cohort group ID),
#' and any additional columns in the source table.
#'
#' @export
spadesCBMdbReadTable <- function(spadesCBMdb, year, table, useCache = TRUE){

  useCache <- useCache & table != "key"

  if (!requireNamespace("qs", quietly = TRUE)) stop(
    "The package \"qs\" is required to read the SpaDES CBM database")
  if (useCache && !requireNamespace("reproducible", quietly = TRUE)) stop(
    "The package \"reproducible\" is required if useCache = TRUE")
  if (useCache && !requireNamespace("withr", quietly = TRUE)) stop(
    "The package \"withr\" is required if useCache = TRUE")

  if (useCache) withr::local_options(c(reproducible.cachePath = file.path(spadesCBMdb, "cache")))
  cacheOrNot <- if (useCache) reproducible::Cache else eval

  .spadesCBMdbReadTable(spadesCBMdb, year, table) |> cacheOrNot()
}

.spadesCBMdbReadTable <- function(spadesCBMdb, year, table){

  dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, "key")

  if (table != "key"){

    keyCols <- c("cohortID", "pixelIndex", "row_idx")
    if (!identical(names(dbTable), keyCols)){
      dbTable <- dbTable[, .(cohortID, pixelIndex, row_idx)]
    }

    dbTable <- data.table::merge.data.table(
      dbTable, .spadesCBMdbReadRaw(spadesCBMdb, year, table),
      by = "row_idx", all.x = TRUE)
    data.table::setcolorder(dbTable, unique(c(keyCols, names(dbTable))))
    data.table::setkey(dbTable, cohortID)
  }

  dbTable
}

