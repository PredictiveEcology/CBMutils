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

  CBMdbReadCohortGroupCount <- function(spadesCBMdb, year){
    groupCount <- .spadesCBMdbReadRaw(spadesCBMdb, year, "key")[, .N, by = row_idx]
    data.table::setkey(groupCount, row_idx)
    groupCount
  }
  CBMdbGetTable <- function(table){

    if (by == "year"){

      groupCount <- CBMdbReadCohortGroupCount(spadesCBMdb, year) |> cacheOrNot()

      tbl <- .spadesCBMdbReadRaw(spadesCBMdb, year, table)
      tbl <- (tbl * groupCount$N)[, lapply(.SD, sum), .SDcols = !c("row_idx")]
      tbl <- cbind(year = year, tbl)
      data.table::setkey(tbl, year)

    }else{

      tbl <- .spadesCBMdbReadTable(spadesCBMdb, year, table) |> cacheOrNot()
    }

    # R CMD check note bypass
    for (colName in setdiff(names(tbl), ls())) assign(colName, NULL)

    tbl
  }

  if (summary %in% c("flux", "pools")){

    dbTable <- CBMdbGetTable(summary)

    if (by == "pixelIndex"){
      dbTable <- dbTable[, lapply(.SD, sum), by = pixelIndex, .SDcols = !c("cohortID", "row_idx")]
    }
  }

  if (summary == "NPP"){

    dbTable <- CBMdbGetTable("flux")[, .(
      NPP = sum(
        DeltaBiomass_AG, DeltaBiomass_BG,
        TurnoverMerchLitterInput, TurnoverFolLitterInput,
        TurnoverOthLitterInput, TurnoverCoarseLitterInput, TurnoverFineLitterInput)
    ), by = by]
  }

  if (summary == "poolTypes"){

    dbTable <- CBMdbGetTable("pools")[, .(
      soil   = sum(AboveGroundVeryFastSoil, BelowGroundVeryFastSoil,
                   AboveGroundFastSoil, BelowGroundFastSoil,
                   AboveGroundSlowSoil, BelowGroundSlowSoil, MediumSoil),
      AGlive = sum(Merch, Foliage, Other),
      BGlive = sum(CoarseRoots, FineRoots),
      snags  = sum(StemSnag, BranchSnag)
    ), by = by]
  }

  if (summary == "totalCarbon"){

    dbTable <- CBMdbGetTable("pools")[, .(
      totalCarbon = sum(
        Merch, Foliage, Other, CoarseRoots, FineRoots,
        AboveGroundVeryFastSoil, BelowGroundVeryFastSoil, AboveGroundFastSoil,
        BelowGroundFastSoil, MediumSoil, AboveGroundSlowSoil, BelowGroundSlowSoil,
        StemSnag, BranchSnag)
    ), by = by]
  }

  if (missing(dbTable)) stop(
    "invalid summary selection: ", summary,
    "Choose one of: 'flux', 'pools', 'NPP', 'poolTypes', 'totalCarbon'")

  data.table::setkeyv(dbTable, by)
  return(dbTable)
}

