
#' Match select
#'
#' @param inputs character. Strings to match
#' @param choices character. Match choices
#' @param allowNA logical. Return NA when no match is found.
#' Otherwise an error will be thrown if any input cannot be matched.
#' @param identical logical. Require identical matches.
#' @param nearMatches list. A list of near matches to allow.
#' Item names must match `inputs` and item contents must be vectors of additional allowable matches.
#' @param funSimplify function.
#' Method for simplifying input and choices strings before matching.
#' By default, the strings will be cast to lower case and white space is trimmed.
#' @param ask logical.
#' If TRUE, prompt the user to choose the correct matches.
#' If FALSE, the function will look for a single match to each input.
#' @param inputTable data.frame. If `ask`, include a table of input attributes
#' to print to the user during the selection prompt.
#' @param choiceTable data.frame. If `ask`, include a table of input attributes
#' to print to the user during the selection prompt.
#'
#' @return integer. Match indexes
#'
#' @rdname matchSelect
#' @keywords internal
#' @importFrom crayon yellow
#' @importFrom knitr kable
.matchSelect <- function(inputs, choices, allowNA = FALSE,
                         identical = TRUE, nearMatches = list(),
                         funSimplify = function(x) trimws(tolower(x)),
                         ask = !identical & interactive(),
                         inputTable = NULL, choiceTable = NULL, choiceTableExtra = NULL){

  # Set input
  inputs <- unname(as.character(inputs))
  if (length(inputs) == 0) return(integer(0))
  if (any(is.na(inputs))) stop("input contains NAs")

  # Set matching choices
  chMatch <- unname(as.character(choices))
  if (!is.null(funSimplify)) chMatch <- funSimplify(chMatch)

  # Select matches
  matchIdx <- lapply(inputs, function(input){

    input <- as.character(c(input, nearMatches[[input]]))
    if (!is.null(funSimplify)) input <- funSimplify(input)

    unique(unlist(
      list(
        identical = unname(which(sapply(chMatch, function(ch_i) any(ch_i %in% input)))),
        partial   = if (!identical) unname(which(
          sapply(chMatch, function(ch_i) any(sapply(input, function(inp_i) grepl(inp_i, ch_i, fixed = TRUE))))
        ))
    )))
  })

  if (allowNA){
    matchIdx <- lapply(matchIdx, function(x) if (length(x) > 0) x else NA_integer_)

  }else if (any(sapply(matchIdx, length) == 0)) stop(
    "0 matches found for: ",
    paste(shQuote(inputs[sapply(matchIdx, length) == 0]), collapse = ", "))

  if (!isTRUE(ask) & any(sapply(matchIdx, length) > 1)) stop(
    "Multiple matches found for: ",
    paste(shQuote(inputs[sapply(matchIdx, length) > 1]), collapse = ", "),
    if (!isTRUE(ask)) "\nRun with ask = TRUE to select matches")

  if (isTRUE(ask)) matchIdx <- lapply(1:length(inputs), function(i){

    chMatch <- matchIdx[[i]]

    if (all(is.na(chMatch))) return(chMatch)

    inPrint <- data.frame(input = inputs[[i]])
    if (!is.null(inputTable)){
      inPrint <- cbind(inPrint, inputTable[i,])
      whichCol <- sapply(inPrint[, 2:ncol(inPrint)], identical, inPrint$input)
      if (sum(whichCol) == 1){
        inPrint[[which(whichCol) + 1]] <- NULL
        names(inPrint)[[1]] <- names(whichCol)[whichCol]
      }
    }

    chPrint <- data.frame(
      rowID = 1:length(chMatch),
      match = choices[chMatch]
    )
    if (!is.null(choiceTable)){
      chPrint  <- cbind(chPrint, choiceTable[chMatch,])
      whichCol <- sapply(chPrint[, 3:ncol(chPrint)], identical, chPrint$match)
      if (sum(whichCol) == 1){
        chPrint[[which(whichCol) + 2]] <- NULL
        names(chPrint)[[2]] <- names(whichCol)[whichCol]
      }
      for (col in which(sapply(chPrint, is.list))){
        chPrint[[col]] <- sapply(chPrint[[col]], function(x){
          paste(unique(unlist(x)), collapse = "; ")
        })
      }
    }

    repeat{

      ans <- readline(cat(paste(c(
        "INPUT TO MATCH:",
        paste(sprintf(paste0("%-", max(nchar(names(inPrint))), "s : %s"),
                      names(inPrint), inPrint),
              collapse = "\n"),
        "",
        "MATCH OPTIONS:",
        knitr::kable(chPrint, format = "simple"),
        "",
        crayon::yellow(paste0(
          "Enter the row ID of the correct match",
          if (!is.null(choiceTableExtra)){
            " or \"more\" to view more information about the choices"
          },
          ": "))
      ), collapse = "\n")))

      if (!is.null(choiceTableExtra) & identical(trimws(tolower(ans)), "more")){

        ans <- readline(cat(paste(c(
          "INPUT TO MATCH:",
          paste(sprintf(paste0("%-", max(nchar(names(inPrint))), "s : %s"),
                        names(inPrint), inPrint),
                collapse = "\n"),
          "",
          "MATCH OPTIONS:",
          knitr::kable(cbind(chPrint, choiceTableExtra[chMatch,]), format = "simple"),
          "",
          crayon::yellow("Enter the row ID of the correct match: ")
        ), collapse = "\n")))
      }
      cat("\n")

      selectID <- suppressWarnings(tryCatch(as.numeric(trimws(ans)), error = function(e) NULL))
      if (isTRUE(selectID %in% chPrint$rowID)) return(chMatch[[selectID]])
    }
  })

  do.call(c, matchIdx)
}

