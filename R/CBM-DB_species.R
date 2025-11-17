
#' Species match
#'
#' Retrieve species metadata by matching species names or other identifiers with columns in \code{sppEquivalencies}.
#'
#' @param species Species identifiers.
#' @param match character. \code{sppEquivalencies} column(s) to match \code{species} with.
#' @param otherNames list. A list of alternative species identified to allow in matching.
#' Item names must match `species` and item contents must be vectors of additional allowable matches.
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
sppMatch <- function(species, match = c("LandR", "Latin_full", "EN_generic_short", "EN_generic_full"),
                     otherNames = NULL, return = NULL, checkNA = !is.null(return),
                     sppEquivalencies = NULL){

  # Read species equivalencies table
  if (is.null(sppEquivalencies)){
    if (length(find.package("LandR", quiet = TRUE)) == 0) stop("LandR package required")
    sppEquivalencies <- LandR::sppEquivalencies_CA
  }
  sppEquiv <- tryCatch(
    as.data.table(sppEquivalencies),
    error = function(e) stop(
      "sppEquivalencies could not be converted to data.table: ", e$message, call. = FALSE))

  # Return 0 rows
  if (length(species) == 0) return(sppEquiv[0,])

  # Check matching columns
  colExists <- c(match, return) %in% names(sppEquiv)
  if (!all(colExists)) stop(
    "column(s) not found in sppEquivalencies: ",
    paste(shQuote(c(match, return)[!colExists]), collapse = ", "))

  if (!is.null(return)){

    # Allow duplicate matches if there is a single unique set of return rows
    ## Create secondary matches from duplicated rows
    sppEquivUQ <- sppEquiv[, lapply(.SD, function(x) list(list(x))), by = return]

    sppEquiv  <- sppEquivUQ[, .SD, .SDcols = c(match, return)]
    sppExtras <- sppEquivUQ[, .SD, .SDcols = setdiff(names(sppEquiv), c(match, return))]
    if (ncol(sppExtras) == 0) sppExtras <- NULL

  }else{
    sppExtras <- NULL
  }

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
  speciesUQ <- unique(species)

  matchIdx <- lapply(sppEquiv[, .SD, .SDcols = match], function(choiceCol){

    if (!is.list(choiceCol)){

      choices <- choiceCol
      nearMatches <- list()

    }else{

      chMatches <- lapply(choiceCol, function(ch){
        ch <- unlist(ch)
        ch <- ch[!is.na(ch)]
        ch <- ch[ch != ""]
        if (length(ch) > 0) ch else NA_character_
      })

      choices <- sapply(chMatches, `[[`, 1)

      nearMatches <- chMatches
      names(nearMatches) <- choices
      nearMatches <- nearMatches[sapply(nearMatches, length) > 1]
    }

    nearMatchNm <- c(names(nearMatches), names(otherNames))
    nearMatches <- lapply(nearMatchNm, function(nm) c(nearMatches[[nm]], otherNames[[nm]]))
    names(nearMatches) <- nearMatchNm

    .matchSelect(
      inputs      = speciesUQ,
      choices     = choices,
      choiceTable = sppEquiv,
      choiceTableExtra = sppExtras,
      identical   = TRUE,
      funSimplify = .chSimple,
      nearMatches = nearMatches,
      allowNA     = TRUE,
      ask         = "never"
    )
  })
  matchIdx <- lapply(1:length(speciesUQ), function(i){
    unique(na.omit(do.call(c, lapply(matchIdx, `[[`, i))))
  })

  if (any(sapply(matchIdx, length) > 1)) stop(
    "specie(s) with multiple matches in sppEquivalencies: ",
    paste(shQuote(unique(speciesUQ[sapply(matchIdx, length) > 1])), collapse = ", "))

  if (any(sapply(matchIdx, length) == 0)) stop(
    "specie(s) not found in sppEquivalencies: ",
    paste(shQuote(unique(speciesUQ[sapply(matchIdx, length) == 0])), collapse = ", "))

  # Set table to return
  sppEquiv <- sppEquiv[unlist(matchIdx)[match(species, speciesUQ)],]
  if (!is.null(return)) sppEquiv <- sppEquiv[, .SD, .SDcols = return]

  # Check for column NAs
  if (checkNA){

    colNA <- is.na(sppEquiv)

    if (any(colNA)) stop(
      "NA(s) found in sppEquivalencies table:\n",
      "Species   : ", paste(shQuote(species[apply(colNA, 1, any)]), collapse = ", "), "\n",
      "Column(s) : ", paste(shQuote(return[ apply(colNA, 2, any)]), collapse = ", "))
  }

  # Return
  return(sppEquiv)
}


