
#' Species match
#'
#' Retrieve species metadata by matching species names or other identifiers with columns in \code{sppEquivalencies}.
#'
#' @param species Species identifiers.
#' @param match character. \code{sppEquivalencies} columns to match \code{species} with.
#' Defaults to \code{LandR::sppEquivalencies_CA} columns with Latin and generic English species names.
#' @param return character. \code{sppEquivalencies} columns to return.
#' All columns will be returned by default.
#' @param checkNA logical. Check for NA values in the returned columns.
#' Defaults to TRUE if the \code{return} argument is used; otherwise FALSE.
#' @param sppEquivalencies data.table. Table of species identifiers and metadata.
#' Defaults to \code{LandR::sppEquivalencies_CA}.
#'
#' @return data.table. Subset of \code{sppEquivalencies} with 1 row per species.
#'
#' @export
#' @importFrom data.table as.data.table
sppMatch <- function(species, match = NULL, return = NULL, checkNA = !is.null(return),
                     sppEquivalencies = NULL){

  # Set matching columns
  if (is.null(match)) match <- c("Latin_full", "EN_generic_short", "EN_generic_full")

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
  colExists <- tolower(c(match, return)) %in% tolower(names(sppEquivalencies))
  if (!all(colExists)) stop(
    "column(s) not found in sppEquivalencies: ",
    paste(shQuote(c(match, return)[!colExists]), collapse = ", "))

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
  matchIdx <- lapply(match, function(mCol){

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

  # Subset table by columns to return
  ## If multiple matches are found only error if they would return a different set of columns.
  if (!is.null(return)){

    sppEquivalencies <- sppEquivalencies[, .SD, .SDcols = return]

    for (i in which(sapply(matchIdx, length) > 1)){
      matchIdx[[i]] <- matchIdx[[i]][[which(!duplicated(sppEquivalencies[matchIdx[[i]],]))]]
    }
  }

  if (any(sapply(matchIdx, length) > 1)) stop(
    "specie(s) with multiple matches in sppEquivalencies: ",
    paste(shQuote(unique(species[sapply(matchIdx, length) > 1])), collapse = ", "))

  if (any(sapply(matchIdx, length) == 0)) stop(
    "specie(s) not found in sppEquivalencies: ",
    paste(shQuote(unique(species[sapply(matchIdx, length) == 0])), collapse = ", "))

  sppMatchTable <- sppEquivalencies[unlist(matchIdx),]

  # Check for column NAs
  if (checkNA){

    colNA <- is.na(sppMatchTable)

    if (any(colNA)) stop(
      "NA(s) found in sppEquivalencies table:\n",
      "Species   : ", paste(shQuote(species[apply(colNA, 1, any)]), collapse = ", "), "\n",
      "Column(s) : ", paste(shQuote(return[ apply(colNA, 2, any)]), collapse = ", "))
  }

  # Return
  return(sppMatchTable)
}


