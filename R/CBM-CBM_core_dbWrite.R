
#' simList: Write SpaDES CBM database
#'
#' Write simulation **CBM_core** module data to the SpaDES CBM database.
#'
#' @template simCBM
#' @param year numeric. Simulation data year.
#' Defaults to the current simulation year.
#' @inheritParams spadesCBMdbWrite
#' @export
simCBMdbWrite <- function(simCBM, year, parameters = TRUE, state = TRUE, flux = TRUE, pools = TRUE){

  if (missing(year)){
    year <- SpaDES.core::convertTimeunit(SpaDES.core::times(simCBM)$current, "year")
  }

  spadesCBMdbWrite(
    simCBM$spadesCBMdb,
    simCBM$cbm_vars,
    year = year,
    parameters = parameters, state = state, flux = flux, pools = pools)
}

#' Write SpaDES CBM database
#'
#' Write **CBM_core** module data to the SpaDES CBM database.
#'
#' @template spadesCBMdb
#' @param parameters logical. Write the 'parameters' table to file.
#' @param state logical. Write the 'state' table to file.
#' @param flux logical. Write the 'flux' table to file.
#' @param pools logical. Write the 'pools' table to file.
#'
#' @keywords internal
spadesCBMdbWrite <- function(spadesCBMdb, cbm_vars, year, parameters = TRUE, state = TRUE, flux = TRUE, pools = TRUE){

  dir.create(dirname(.spadesCBMdbDataPath(spadesCBMdb, year, "key")), recursive = TRUE, showWarnings = FALSE)

  for (table in c(
    "key",
    "parameters"[parameters],
    "state"[state],
    "flux"[flux],
    "pools"[pools])
  ) qs::qsave(cbm_vars[[table]], .spadesCBMdbDataPath(spadesCBMdb, year, table))

  return(invisible())
}

# Returns path to a CBM outputs DB file
.spadesCBMdbDataPath <- function(spadesCBMdb, year, table){

  if (is.null(spadesCBMdb)) stop("spadesCBMdb is NULL")
  if (is.null(year))        stop("year is NULL")
  if (is.null(table))       stop("table is NULL")

  file.path(spadesCBMdb, "data", paste0(year, "_", table, ".qs"))
}

