utils::globalVariables(c(
  "..id_col", "age", "value", "valueNumeric", "variable", "set"
))

#' Makes plots of all columns for each id_col ID
#'
#' @param inc increment data
#' @param id_col column by which to group plots
#' @param title Title of plots
#'
#' @export
#' @importFrom data.table copy melt set
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom ggplot2 aes element_text geom_line ggplot labs theme theme_bw
#' @importFrom patchwork wrap_plots plot_layout plot_annotation
m3ToBiomPlots <- function(inc, id_col = "gcids", title) {

  gInc <- copy(inc)
  if ("id"      %in% names(gInc)) gInc[, id      := NULL]
  if ("ecozone" %in% names(gInc)) gInc[, ecozone := NULL]

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

  plotList <- list()

  for (i in seq_along(plotPages)) {
    plotList[[i]] <- wrap_plots(plotlist = plotPages[[i]], ncol = 3) +
     plot_layout(guides = "collect") +
     plot_annotation(title = paste(title, "- Page", i)) &
     theme(legend.position = "right")
  }
  return(plotList)
}

