
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
simCBMdbReadSummary <- function(simCBM, summary, by = "cohortID", units = "t/ha",
                                year = NULL, years = NULL, useCache = TRUE){

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
    units    = units,
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
#' @param units character. 't/ha', 't', or 'Mt'.
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
spadesCBMdbReadSummary <- function(spadesCBMdb, summary, by = "cohortID", units = "t/ha",
                                   year = NULL, years = NULL, useCache = TRUE){

  if (!requireNamespace("qs2", quietly = TRUE)) stop(
    "The package \"qs2\" is required to read the SpaDES CBM database")
  if (useCache && !requireNamespace("reproducible", quietly = TRUE)) stop(
    "The package \"reproducible\" is required if useCache = TRUE")
  if (useCache && !requireNamespace("withr", quietly = TRUE)) stop(
    "The package \"withr\" is required if useCache = TRUE")

  if (useCache) withr::local_options(c(reproducible.cachePath = file.path(spadesCBMdb, "cache")))
  cacheOrNot <- if (useCache) reproducible::Cache else eval

  if (by == "year"){

    if (is.null(years)) stop("'years' argument required")

    sumTbl <- data.table::rbindlist(
      lapply(years, function(year){
        .spadesCBMdbReadSummary(spadesCBMdb, year, summary, by, units, useCache) |> cacheOrNot()
      }),
      fill = TRUE)
    data.table::setkey(sumTbl, year)
    sumTbl

  }else{

    .spadesCBMdbReadSummary(spadesCBMdb, year, summary, by, units, useCache) |> cacheOrNot()
  }
}

.spadesCBMdbReadSummary <- function(spadesCBMdb, year, summary, by = "cohortID", units = "t/ha", useCache = TRUE){

  if (missing(summary)) summary <- NULL

  if (!by %in% c("cohortID", "pixelIndex", "year")) stop(
    "data can be summarized by 'cohortID', 'pixelIndex', or 'year'")
  if (!units %in% c("t/ha", "t", "Mt")) stop(
    "data can be summarized by units 't/ha', 't', or 'Mt'")

  if (useCache) withr::local_options(c(reproducible.cachePath = file.path(spadesCBMdb, "cache")))
  cacheOrNot <- if (useCache) reproducible::Cache else eval

  # Get cohort group data
  if (summary %in% c("flux", "pools")){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, summary)
  }

  if (summary == "emissions"){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, "flux")

    dbTable <- dbTable[, .(
      row_idx,
      CO2 = rowSums(dbTable[, .(DisturbanceBioCO2Emission, DecayDOMCO2Emission, DisturbanceDOMCO2Emission)]),
      CH4 = rowSums(dbTable[, .(DisturbanceBioCH4Emission, DisturbanceDOMCH4Emission)]),
      CO  = rowSums(dbTable[, .(DisturbanceBioCOEmission,  DisturbanceDOMCOEmission)])
    )]
  }

  if (summary == "NPP"){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, "flux")

    dbTable <- dbTable[, .(
      row_idx,
      NPP = rowSums(dbTable[, .(
        DeltaBiomass_AG, DeltaBiomass_BG,
        TurnoverMerchLitterInput, TurnoverFolLitterInput,
        TurnoverOthLitterInput, TurnoverCoarseLitterInput, TurnoverFineLitterInput
      )])
    )]
  }

  if (summary == "totalCarbon"){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, "pools")

    dbTable <- dbTable[, .(
      row_idx,
      totalCarbon = rowSums(dbTable[, .(
        Merch, Foliage, Other, CoarseRoots, FineRoots,
        AboveGroundVeryFastSoil, BelowGroundVeryFastSoil, AboveGroundFastSoil,
        BelowGroundFastSoil, MediumSoil, AboveGroundSlowSoil, BelowGroundSlowSoil,
        StemSnag, BranchSnag
      )])
    )]
  }

  if (summary == "products"){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, "pools")

    dbTable <- dbTable[, .(row_idx, Products)]
  }

  if (summary == "poolTypes"){

    dbTable <- .spadesCBMdbReadRaw(spadesCBMdb, year, "pools")

    dbTable <- dbTable[, .(
      row_idx,
      Soil = rowSums(dbTable[, .(
        AboveGroundVeryFastSoil, BelowGroundVeryFastSoil,
        AboveGroundFastSoil, BelowGroundFastSoil,
        AboveGroundSlowSoil, BelowGroundSlowSoil, MediumSoil
      )]),
      BGlive = rowSums(dbTable[, .(CoarseRoots, FineRoots)]),
      AGlive = rowSums(dbTable[, .(Merch, Foliage, Other)]),
      Snags  = rowSums(dbTable[, .(StemSnag, BranchSnag)])
    )]
  }

  if (missing(dbTable)) stop(
    "invalid summary selection: ", summary,
    "Choose one of: 'flux', 'pools', 'emissions', 'NPP', 'totalCarbon', 'products', 'poolTypes'")

  if (units == "Mt"){
    cols <- setdiff(names(dbTable), "row_idx")
    dbTable[, (cols) := lapply(.SD, function(x) x / 10^6), .SDcols = cols]
  }

  if (by == "year" & units != "t/ha"){

    rowArea <- .spadesCBMdbReadRaw(spadesCBMdb, year, "state")$area

    dbTable[, row_idx := NULL]
    dbTable <- dbTable[, lapply(.SD, function(x) sum(x * rowArea))]
    dbTable$year <- year

  }else{

    dbTable <- merge(
      .spadesCBMdbReadRaw(spadesCBMdb, year, "key")[, .(cohortID, pixelIndex, row_idx)],
      dbTable, by = "row_idx")
    dbTable[, row_idx := NULL]

    if (units != "t/ha"){

      pixelAreas <- .spadesCBMdbReadPixelAreas(spadesCBMdb, year) |> cacheOrNot(verbose = FALSE)
      dbTable <- merge(dbTable, pixelAreas, by = "pixelIndex")

      cols <- setdiff(names(dbTable), c("cohortID", "pixelIndex", "area"))
      dbTable[, (cols) := lapply(.SD, function(x) x * area), .SDcols = cols]
      dbTable[, area := NULL]
    }

    if (by != "cohortID"){
      dbTable[, cohortID := NULL]
      dbTable <- dbTable[, lapply(.SD, sum), by = "pixelIndex"]
    }
    if (by == "year"){
      dbTable[, pixelIndex := NULL]
      dbTable <- dbTable[, lapply(.SD, mean)]
      dbTable$year <- year
    }
  }

  data.table::setkeyv(dbTable, by)
  data.table::setcolorder(dbTable)
  return(dbTable)
}

.spadesCBMdbReadPixelAreas <- function(spadesCBMdb, year){

  pixelAreas <- merge(
    .spadesCBMdbReadRaw(spadesCBMdb, year, "key")[, .(pixelIndex, row_idx)],
    .spadesCBMdbReadRaw(spadesCBMdb, year, "state")[, .(row_idx, area)],
    by = "row_idx")
  pixelAreas[, area := area / .N, by = row_idx]

  data.table::setkey(pixelAreas, pixelIndex)
  pixelAreas[, .(pixelIndex, area)]
}



