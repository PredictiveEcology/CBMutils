
#' simList: Read SpaDES CBM database
#'
#' Read a raw data table from a simulation's **CBM_core** SpaDES CBM database.
#'
#' @template simCBM
#' @param year numeric. Year of simulation results.
#' Defaults to the current simulation year.
#' @inheritParams spadesCBMdbReadRaw
#' @inherit spadesCBMdbReadRaw return
#' @export
simCBMdbReadRaw <- function(simCBM, year, table, ...){

  if (missing(year)){
    year <- SpaDES.core::convertTimeunit(SpaDES.core::times(simCBM)$current, "year")
  }

  spadesCBMdbReadRaw(simCBM$spadesCBMdb, year = year, table = table, ...)
}

#' Read CBM SpaDES CBM database
#'
#' Read a raw data table from a **CBM_core** SpaDES CBM database.
#'
#' @template spadesCBMdb
#' @param year numeric. Year of simulation results.
#' @param table character. Name of database table to read.
#' One of 'key', 'parameters', 'state', 'flux', or 'pools'.
#' @param ... not used
#'
#' @return data.table
#'
#' @export
spadesCBMdbReadRaw <- function(spadesCBMdb, year, table, ...){

  if (!requireNamespace("qs", quietly = TRUE)) stop(
    "The package \"qs\" is required to read the SpaDES CBM database")

  .spadesCBMdbReadRaw(spadesCBMdb, year, table)
}

.spadesCBMdbReadRaw <- function(spadesCBMdb, year, table){

  qsPath <- .spadesCBMdbDataPath(spadesCBMdb, year, table)
  if (!file.exists(qsPath)) stop(
    "SpaDES CBM database file not found for year ", year, ": ", qsPath,
    "\nSee CBM_core module parameters to customize simulation saving behaviour")

  qs::qread(qsPath)
}

