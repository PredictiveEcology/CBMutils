
# Get simList times in years without using SpaDES.core functions
simYears <- function(sim){

  simTimes <- sim@simtimes[c("current", "start", "end")]
  simTimes <- lapply(simTimes, function(st){
    if (attr(st, "unit") == "second"){
      st <- st / (60 * 60 * 24 * 365.25)
      attr(st, "unit") <- "year"
    }
    return(st)
  })

  if (!all(sapply(simTimes, attr, "unit") == "year") |
      any(simTimes$start > c(sim$simTimes$current, sim$simTimes$end)) |
      any(simTimes$end   < c(sim$simTimes$current, sim$simTimes$start))) stop(
        "Converting sim@simtimes to unit 'year' failed")

  return(simTimes)
}

