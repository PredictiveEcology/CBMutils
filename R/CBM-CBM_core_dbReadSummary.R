utils::globalVariables(c("cohortID", "pixelIndex", "row_idx"))

#' simList: Read SpaDES CBM database summary
#'
#' Read a data summary from a simulation's **CBM_core** SpaDES CBM database.
#'
#' @template simCBM
#' @param year numeric. Year of simulation results.
#' Required if `by = 'cohortID' or 'pixelIndex'`.
#' Defaults to the current simulation year.
#' @param years numeric. Years of simulation results to summarize.
#' Required if `by = 'year'`.
#' Defaults to the full simulation time span.
#' @inheritParams spadesCBMdbReadSummary
#' @inherit spadesCBMdbReadSummary return
#' @export
simCBMdbReadSummary <- function(simCBM, summary, by = "cohortID", year = NULL, years = NULL, useCache = TRUE){

  if (is.null(year) & by != "year"){
    year <- SpaDES.core::convertTimeunit(SpaDES.core::times(simCBM)$current, "year")
  }
  if (is.null(years) & by == "year"){
    simTimes <- lapply(SpaDES.core::times(simCBM)[c("start", "end")], SpaDES.core::convertTimeunit, "year")
    years <- simTimes$start:simTimes$end
  }
  spadesCBMdbReadSummary(
    simCBM$spadesCBMdb,
    summary  = summary,
    by       = by,
    year     = year,
    years    = years,
    useCache = useCache)
}

#' Read CBM SpaDES CBM database summary
#'
#' Read a data summary from a **CBM_core** SpaDES CBM database.
#'
#' @template spadesCBMdb
#' @param summary character. Name of summary table to collect.
#' @param by character. Name of field to summarize results by.
#' Data can be summarized by 'cohortID', 'pixelIndex', or 'year'
#' @param year numeric. Year of simulation results.
#' Required if `by = 'cohortID' or 'pixelIndex'`.
#' @param years numeric. Years of simulation results to summarize.
#' Required if `by = 'year'`.
#' @param useCache logical. Cache database query result to the database's
#' `cache` directory with \code{\link[reproducible]{Cache}}.
#'
#' @return data.table with key matching the `by` argument.
#'
#' @export
spadesCBMdbReadSummary <- function(spadesCBMdb, summary, by = "cohortID", year = NULL, years = NULL, useCache = TRUE){

  if (!requireNamespace("qs", quietly = TRUE)) stop(
    "The package \"qs\" is required to read the SpaDES CBM database")
  if (useCache && !requireNamespace("reproducible", quietly = TRUE)) stop(
    "The package \"reproducible\" is required if useCache = TRUE")
  if (useCache && !requireNamespace("withr", quietly = TRUE)) stop(
    "The package \"withr\" is required if useCache = TRUE")

  if (useCache) withr::local_options(c(reproducible.cachePath = file.path(spadesCBMdb, "cache")))
  cacheOrNot <- if (useCache) reproducible::Cache else eval

  if (by == "year"){

    sumTbl <- data.table::rbindlist(
      lapply(years, function(year){
        .spadesCBMdbReadSummary(spadesCBMdb, year, summary, by, useCache) |> cacheOrNot()
      }),
      fill = TRUE)
    data.table::setkey(sumTbl, year)
    sumTbl

  }else{

    .spadesCBMdbReadSummary(spadesCBMdb, year, summary, by, useCache) |> cacheOrNot()
  }
}

.spadesCBMdbReadSummary <- function(spadesCBMdb, year, summary, by = "cohortID", useCache = TRUE){

  if (missing(summary)) summary <- NULL

  if (!by %in% c("cohortID", "pixelIndex", "year")) stop(
    "data can be summarized by 'cohortID', 'pixelIndex', or 'year'")

  if (useCache) withr::local_options(c(reproducible.cachePath = file.path(spadesCBMdb, "cache")))
  cacheOrNot <- if (useCache) reproducible::Cache else eval

  # Get cohort group data
  if (summary %in% c("flux", "pools")){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, summary)
  }

  if (summary == "NPP"){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, "flux")

    # R CMD check note bypass
    for (colName in setdiff(names(dbTable), ls())) assign(colName, NULL)

    dbTable <- dbTable[, .(row_idx, NPP = rowSums(dbTable[, .(
      DeltaBiomass_AG, DeltaBiomass_BG,
      TurnoverMerchLitterInput, TurnoverFolLitterInput,
      TurnoverOthLitterInput, TurnoverCoarseLitterInput, TurnoverFineLitterInput
    )]))]
  }

  if (summary == "poolTypes"){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, "pools")

    # R CMD check note bypass
    for (colName in setdiff(names(dbTable), ls())) assign(colName, NULL)

    dbTable <- dbTable[, .(
      soil   = sum(AboveGroundVeryFastSoil, BelowGroundVeryFastSoil,
                   AboveGroundFastSoil, BelowGroundFastSoil,
                   AboveGroundSlowSoil, BelowGroundSlowSoil, MediumSoil),
      AGlive = sum(Merch, Foliage, Other),
      BGlive = sum(CoarseRoots, FineRoots),
      snags  = sum(StemSnag, BranchSnag)
    ), by = row_idx]
  }

  if (summary == "totalCarbon"){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, "pools")

    # R CMD check note bypass
    for (colName in setdiff(names(dbTable), ls())) assign(colName, NULL)

    dbTable <- dbTable[, .(row_idx, totalCarbon = rowSums(dbTable[, .(
      Merch, Foliage, Other, CoarseRoots, FineRoots,
      AboveGroundVeryFastSoil, BelowGroundVeryFastSoil, AboveGroundFastSoil,
      BelowGroundFastSoil, MediumSoil, AboveGroundSlowSoil, BelowGroundSlowSoil,
      StemSnag, BranchSnag
    )]))]
  }

  if (missing(dbTable)) stop(
    "invalid summary selection: ", summary,
    "Choose one of: 'flux', 'pools', 'NPP', 'poolTypes', 'totalCarbon'")

  # Summarize
  if (by == "year"){

    CBMdbReadCohortGroupCount <- function(spadesCBMdb, year){
      groupCount <- .spadesCBMdbReadRaw(spadesCBMdb, year, "key")[, .N, by = row_idx]
      data.table::setkey(groupCount, row_idx)
      groupCount
    }
    groupCount <- CBMdbReadCohortGroupCount(spadesCBMdb, year) |> cacheOrNot()

    dbTable <- (dbTable * groupCount$N)[, lapply(.SD, sum), .SDcols = !c("row_idx")]
    dbTable <- cbind(year = year, dbTable)

  }else{

    dbTable <- data.table::merge.data.table(
      .spadesCBMdbReadRaw(spadesCBMdb, year, "key")[
        , .SD, .SDcols = unique(c(by, "pixelIndex", "row_idx"))],
      dbTable, by = "row_idx")
    dbTable[, row_idx := NULL]

    if (by == "pixelIndex") dbTable <- dbTable[, lapply(.SD, sum), by = "pixelIndex"]
  }

  data.table::setkeyv(dbTable, by)
  return(dbTable)
}

