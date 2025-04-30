
#' Species match
#'
#' Retrieve species metadata by matching species names or other identifiers with columns in \code{sppEquivalencies}.
#'
#' @param species Species identifiers.
#' @param matchCol character. \code{sppEquivalencies} columns to match \code{species} with.
#' Defaults to \code{LandR::sppEquivalencies_CA} columns with Latin and generic English species names.
#' @param sppEquivalencies data.table. Table of species identifiers and metadata.
#' Defaults to \code{LandR::sppEquivalencies_CA}.
#' @param checkNA character. \code{sppEquivalencies} columns to check for NA values in.
#'
#' @return data.table. Subset of \code{sppEquivalencies} with 1 row per species.
#'
#' @export
#' @importFrom data.table as.data.table
sppMatch <- function(species, matchCol = NULL, sppEquivalencies = NULL,
                     checkNA = c("CBM_speciesID", "Broadleaf")){

  # Set matching columns
  if (is.null(matchCol)) matchCol <- c("Latin_full", "EN_generic_short", "EN_generic_full")

  # Check for NAs
  if (length(species) > 0 && any(is.na(species))) stop("species contains NAs")

  # Read species equivalencies table
  if (is.null(sppEquivalencies)) sppEquivalencies <- LandR::sppEquivalencies_CA
  sppEquivalencies <- tryCatch(
    as.data.table(sppEquivalencies),
    error = function(e) stop(
      "sppEquivalencies could not be converted to data.table: ", e$message, call. = FALSE))

  # Return 0 rows
  if (length(species) == 0) return(sppEquivalencies[0,])

  # Check that required columns are available
  colExists <- tolower(c(matchCol, checkNA)) %in% tolower(names(sppEquivalencies))
  if (!all(colExists)) stop(
    "column(s) not found in sppEquivalencies: ",
    paste(shQuote(c(matchCol, checkNA)[!colExists]), collapse = ", "))

  # Set function for matching character columns
  ## All character lower case
  ## Remove leading and trailing white space
  ## Remove all punctuation
  ## Remove "'s" on species names where (.e.g "Engelmann's spruce" -> "Engelmann spruce")
  .chSimple <- function(ch){
    ch <- sub("'s ", " ", ch, fixed = TRUE)
    gsub("[[:punct:]]*", "", trimws(tolower(as.character(ch))))
  }

  # Match allowing multiples
  matchIdx <- lapply(matchCol, function(mCol){

    matchTo <- sppEquivalencies[[which(tolower(names(sppEquivalencies)) == tolower(mCol))]]

    if (is.character(matchTo)){
      matchTo <- .chSimple(matchTo)
      matchIn <- .chSimple(species)

    }else{
      matchIn <- as(species, class(matchTo))
    }

    lapply(matchIn, function(mIn) which(mIn == matchTo))
  })
  matchIdx <- lapply(1:length(species), function(i){
    unique(do.call(c, lapply(matchIdx, `[[`, i)))
  })

  if (any(sapply(matchIdx, length) == 0)) stop(
    "specie(s) not found in sppEquivalencies: ",
    paste(shQuote(unique(species[sapply(matchIdx, length) == 0])), collapse = ", "))

  if (any(sapply(matchIdx, length) > 1)) stop(
    "specie(s) with multiple matches in sppEquivalencies: ",
    paste(shQuote(unique(species[sapply(matchIdx, length) > 1])), collapse = ", "))

  sppMatchTable <- sppEquivalencies[unlist(matchIdx), ]

  # Check for column NAs
  if (!is.null(checkNA)){

    colNA <- is.na(sppMatchTable[, .SD, .SDcols = checkNA])

    if (any(colNA)) stop(
      "NA(s) found in sppEquivalencies table:\n",
      "Species   : ", paste(shQuote(species[apply(colNA, 1, any)]), collapse = ", "), "\n",
      "Column(s) : ", paste(shQuote(checkNA[apply(colNA, 2, any)]), collapse = ", "))
  }

  # Return
  return(sppMatchTable)
}


