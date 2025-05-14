utils::globalVariables(c(
  "..id_col", "age", "value", "valueNumeric", "variable", "set"
))

#' Plot all columns that are not id_col
#'
#' @param inc DESCRIPTION NEEDED
#' @param id_col DESCRIPTION NEEDED
#'
#' @export
#' @importFrom data.table copy melt
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom ggplot2 aes element_text geom_line ggplot labs theme theme_bw
#' @importFrom patchwork wrap_plots plot_layout plot_annotation
m3ToBiomPlots <- function(inc, id_col = "gcids") {
  gInc <- copy(inc)
  colsToRemove <- c("id", "ecozone")
  gInc <- gInc[, (colsToRemove) := NULL]
  gc <- data.table::melt(gInc, id.vars = c(id_col, "age"))

  gc[is.na(value), "value"] <- 0
  set(gc, NULL, "valueNumeric", as.numeric(gc$value))

  splitGc <- split(gc, gc[[id_col]])
  plots <- lapply(names(splitGc), function(name) {
    data <- splitGc[[name]]
    ggplot(data, aes(x = age, y = valueNumeric, group = variable, color = variable)) +
      geom_line() +
      theme_bw() +
      labs(title = name) +
      theme(plot.title = element_text(hjust = 0.5))
  })

  plotPages <- split(plots, ceiling(seq_along(plots) / 6))

  Plots <- list()

  for (i in seq_along(plotPages)) {
   Plots[[i]] <- wrap_plots(plotlist = plotPages[[i]], ncol = 3) +
     plot_layout(guides = "collect") +
     plot_annotation(title = paste("Cumulative merch fol other by gcid - Page", i)) &
     theme(legend.position = "right")
  }
  return(Plots)
}

