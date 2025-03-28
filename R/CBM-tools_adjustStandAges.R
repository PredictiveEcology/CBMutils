
utils::globalVariables(c(
  "year", "age", "ageCalc"
))

#' Adjust stand ages
#'
#' Adjust stand ages to represent a year before or after a given date.
#' Optionally include disturbance events that reset ages to year 0.
#'
#' @param standAges data.table. Table of stand ages
#' with an ID column and the numeric columns 'age'.
#' Optionally include a 'delay' column with regeneration delays for each stand.
#' A standard delay can also be provided with the \code{delay} argument.
#' Optionally include a 'default' column with the default age for the stand
#' if it cannot be otherwise calculated.
#' A default age can also be provided with the \code{default} argument.
#' @param yearInput integer. The year that the 'ages' column in \code{standAges} represents.
#' @param yearOutput integer. The year that stand ages must be adjusted to.
#' @param disturbanceEvents data.table. Optional.
#' Table of disturbance events with the ID column in \code{standAges}
#' and another column 'year' of when the disturbance occurred.
#' This can also include rows for years that stands were first established.
#' @param delay integer. Optional. Regeneration delay after a disturbance event.
#' @param default integer. A default age for stands is otherwise unknown.
#' If \code{yearOutput} precedes \code{yearInput},
#' the age cannot be calculated for stands that have disturbances between
#' \code{yearOutput} and \code{yearInput} but none before \code{yearOutput}.
#' \code{default} will be assigned in these cases.
#' NOTE: if a stand has an age lesser than the difference in these years,
#' it is assumed that a disturbance event must have occurred when age == 0.
#' \code{default} will be assigned in these cases.
#' @param warn logical. Warn if ages cannot be calculated
#' or if the provided disturbances do not match the input data.
#'
#' @return \code{standAges} with ages adjusted to \code{yearOutput}.
#'
#' @importFrom data.table as.data.table key setkeyv
#' @export
adjustStandAges <- function(standAges, yearInput, yearOutput, disturbanceEvents = NULL,
                            delay = NULL, default = NULL, warn = TRUE){

  yearInput  <- as.integer(yearInput)
  yearOutput <- as.integer(yearOutput)

  if (length(yearInput)  != 1) stop("'yearInput' must be length 1")
  if (length(yearOutput) != 1) stop("'yearOutput' must be length 1")

  # Read input ages
  standAges <- data.table::as.data.table(standAges)
  if (!"age" %in% names(standAges)){
    if ("ages" %in% names(standAges)){
      standAges$age <- standAges$ages
    }else stop("'standAges' must have column 'age'")
  }

  ## Set table key
  ageKey <- data.table::key(standAges)
  if (is.null(ageKey)){
    ageKey <- setdiff(names(standAges), "age")[[1]]
    data.table::setkeyv(standAges, ageKey)
  }

  if (yearInput == yearOutput) return(standAges[, c(ageKey, "age"), with = FALSE])

  # Initiate table of adjusted ages
  ageAdjust <- copy(subset(standAges, !is.na(age)))[
    , intersect(names(standAges), c(ageKey, "age", "default", "delay")), with = FALSE]
  names(ageAdjust)[[1]] <- "id"

  # Add delays to input table
  if (!is.null(disturbanceEvents)){
    if (!is.null(delay)) ageAdjust$delay <- delay
    if (!"delay" %in% names(ageAdjust)) ageAdjust$delay <- 0
  }

  # Add default age to input table
  if (!is.null(default)) ageAdjust$default <- default

  # Adjust ages as if no disturbances
  ageAdjust[, age := age + yearOutput - yearInput]

  if (!is.null(disturbanceEvents)){

    # Read disturbance events
    disturbanceEvents <- data.table::as.data.table(disturbanceEvents)
    if (!ageKey %in% names(disturbanceEvents)) stop("'disturbanceEvents' must have column '", ageKey, "'")
    if (!"year" %in% names(disturbanceEvents)) stop("'disturbanceEvents' must have column 'year'")

    # Filter disturbance events by relevant IDs
    if (ageKey != "id") disturbanceEvents$id <- disturbanceEvents[[ageKey]]
    disturbanceEvents <- disturbanceEvents[id %in% ageAdjust$id,]

    if (yearOutput > yearInput){

      # Find the most recent disturbance before the year required
      lastEvent <- disturbanceEvents[year %in% yearInput:(yearOutput - 1), ][, .(id, year)]

      if (nrow(lastEvent) > 0){

        lastEvent <- unique(lastEvent[, year := max(year), by = "id"])

        # Add delays to table
        lastEvent <- merge(lastEvent, ageAdjust[, .(id, delay)], by = "id", all.x = TRUE)

        # Calculate the age as the number of years after the disturbance
        lastEvent[, age := yearOutput - (year + delay)]
        lastEvent[age < 0, age := 0]

        # Apply new ages
        ageAdjust[match(lastEvent$id, ageAdjust$id), age := lastEvent$age]

        rm(lastEvent)
      }
    }

    if (yearOutput < yearInput){

      # Determine which pixels were disturbed within time frame
      firstEvent <- disturbanceEvents[year %in% yearOutput:(yearInput - 1),][, .(id, year)]

      # Add events when stands were established
      negAges <- subset(ageAdjust, age < 0 & !id %in% firstEvent$id)
      if (nrow(negAges) > 0){

        negAges[, year := yearOutput - age]

        firstEvent <- rbind(firstEvent, negAges[, .(id, year)])
        rm(negAges)
      }

      if (nrow(firstEvent) > 0){

        firstEvent <- unique(firstEvent[, year := min(year), by = "id"])

        # Add delays to table
        firstEvent <- merge(firstEvent, ageAdjust[, .(id, delay)], by = "id", all.x = TRUE)

        # Find most recent event before year output
        prevEvent <- disturbanceEvents[year <= yearOutput,][, .(id, year)]
        if (nrow(prevEvent) > 0){

          prevEvent <- unique(prevEvent[, prev := max(year), by = "id"])

          firstEvent <- merge(firstEvent, prevEvent[, .(id, prev)], by = "id", all.x = TRUE)
          rm(prevEvent)

        }else firstEvent$prev <- NA_real_

        # Calculate age
        firstEvent[, age := yearOutput - (prev + delay)]
        firstEvent[age < 0, age := 0]

        # Find pixels where a previous disturbance cannot be used to set age
        ## Set age to -1 to be replaced later
        firstEvent[is.na(age), age := -1]

        # Apply new ages
        ageAdjust[match(firstEvent$id, ageAdjust$id), age := firstEvent$age]
      }

      # Check that undisturbed stand ages match historical disturbance data
      if (warn){

        currEvent <- disturbanceEvents[year < yearInput & !id %in% firstEvent$id,]

        if (nrow(currEvent) > 0){

          # Find the most recent disturbance before the input year
          currEvent <- unique(currEvent[, year := max(year), by = "id"])

          # Calculate the age the stand "should" be
          currEvent$ageDist <- yearInput - currEvent$year

          # Compare with input ages
          ageMismatch <- subset(
            merge(currEvent, ageAdjust[, .(id, age)], by = "id", all.x = TRUE),
            age > ageDist)

          if (nrow(ageMismatch) > 0) warning(
            nrow(ageMismatch),
            " stand(s) with age(s) too high to match historic disturbances")
        }
        rm(currEvent)
      }

      rm(firstEvent)
    }
  }

  # Apply default age
  if (yearOutput < yearInput && any(ageAdjust$age < 0)){

    if ("default" %in% names(ageAdjust)){

      ageAdjust[age < 0, age := default]

    }else{

      ageAdjust[age < 0, age := NA]

      if (warn) warning(sum(is.na(ageAdjust$age)), " stand(s) lost due to missing historical events.")
    }
  }

  # Set key and return
  names(ageAdjust)[[1]] <- ageKey
  ageAdjust <- rbind(
    ageAdjust[, c(ageKey, "age"), with = FALSE],
    standAges[is.na(age), c(ageKey, "age"), with = FALSE]
  )
  data.table::setkeyv(ageAdjust, ageKey)
  ageAdjust
}


