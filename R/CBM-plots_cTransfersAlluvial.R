utils::globalVariables(c(
  "labelX", "labelY", "proportion", "sink_pool", "sink_pool_category", "source_pool", "stratum",
  "disturbance_type_id", "disturbance_matrix_id", "spatial_unit_id", "sw_hw", "name", "description"
))

#' `cTransfersAlluvial`
#'
#' @description
#' Maps proportions of carbon transfers across pools during a disturbance.
#'
#' @param cTransfers TODO
#' @param distMatrixID disturbance_matrix_id of the disturbance to plot.
#' @param distName disturbance name you wish to plot. Required only if `distMatrixID` is not provided.
#' @param spuID CBM-CFS3 spatial unit ID of the disturbance to plot. Required only if `distMatrixID` is not provided.
#' @param sw logical. Softwood (TRUE) or hardwood (FALSE). Required only if `distMatrixID` is not provided.
#' @inheritParams .matchSelect
#' @param nearMatches logical. Allow for near matches; e.g. "clearcut" can match "clear-cut".
#' @param ... additional arguments to \code{\link{.matchSelect}}
#'
#' @return alluvialDist Alluvial plot of a disturbance in a specific spatial unit.
#'
#' @export
#' @importFrom data.table as.data.table fifelse
#' @importFrom ggalluvial geom_alluvium geom_stratum
#' @importFrom ggplot2 ggplot aes element_blank scale_fill_manual labs theme_minimal scale_x_discrete geom_text ggplot_build
cTransfersAlluvial <- function(cTransfers, distMatrixID = NULL,
                               distName = NULL, spuID = NULL, sw = NULL,
                               nearMatches = TRUE, identical = !ask, ask = interactive(), ...) {

  if (is.null(distMatrixID)){

    if (is.null(distName) | is.null(spuID) | is.null(sw)) stop(
      "distName, spuID, and sw arguments required if distMatrixID is not provided")

    cTransfersSelect <- unique(
      subset(cTransfers, spatial_unit_id %in% spuID & (sw_hw == "sw") %in% sw)[
        , .(disturbance_matrix_id, disturbance_type_id, name, description)]
    )
    if (nrow(cTransfersSelect) == 0) stop("cTransfers not found for spatial_unit_id = ", spuID, " and sw = ", sw)

    # Set near matches
    if (nearMatches) nearMatches <- list(
      `clearcut` = c("clear cut", "clear-cut"),
      `wildfire` = c("wild fire", "wild-fire")
    )

    distMatrixID <- cTransfersSelect$disturbance_matrix_id[
      .matchSelect(
        inputs      = distName,
        choices     = cTransfersSelect$name,
        choiceTable = cTransfersSelect[, .(disturbance_matrix_id, name)],
        choiceTableExtra = cTransfersSelect[, .(disturbance_type_id, description)],
        identical   = identical,
        nearMatches = nearMatches,
        ask         = ask,
        ...
      )]
  }

  #subset transfer table to only included needed disturbance
  disturbanceTransfers <- cTransfers[disturbance_matrix_id == distMatrixID]
  if (nrow(disturbanceTransfers) == 0) stop("cTransfers not found for disturbance_matrix_id = ", distMatrixID)

  #create pool categories
  disturbanceTransfers[, sink_pool_category := fifelse(sink_pool %in% c("CO2", "CH4", "CO"),
                                                       "Emission",
                                                       fifelse(sink_pool %in% c("BranchSnag", "StemSnag",
                                                                                "BelowGroundSlowSoil", "AboveGroundSlowSoil"),
                                                               "Slow",
                                                               fifelse(sink_pool == "MediumSoil",
                                                                       "Medium",
                                                                       fifelse(sink_pool %in% c("BelowGroundFastSoil", "AboveGroundFastSoil"),
                                                                               "Fast",
                                                                               fifelse(sink_pool %in% c("BelowGroundVeryFastSoil", "AboveGroundVeryFastSoil"),
                                                                                       "Very fast",
                                                                                       fifelse(sink_pool %in% c("Products"),
                                                                                               "Products",
                                                                                               NA_character_))))))]

  #build the plot
  plot <- ggplot(disturbanceTransfers,
                 aes(axis1 = source_pool, axis2 = sink_pool, y = proportion)) +
    geom_alluvium(aes(fill = sink_pool_category), width = 0.02) +
    geom_stratum(width = 0.02)

  #label formatting
  nudge <- 0.01
  stratumLabels <- ggplot_build(plot)$data[[2]]
  stratumLabels$side <- ifelse(stratumLabels$x < mean(range(stratumLabels$x)), "left", "right")
  stratumLabels$labelX <- ifelse(
    stratumLabels$side == "left",
    stratumLabels$xmin - nudge,
    stratumLabels$xmax + nudge
  )
  stratumLabels$labelY <- (stratumLabels$ymin + stratumLabels$ymax) / 2

  #align the strata labels
  stratumLabels$labelhjust <- ifelse(
    stratumLabels$side == "left",
    1,
    0
  )

  #plot formatting
  alluvialDist <- plot +
    geom_text(
      data = stratumLabels,
      aes(x = labelX, y = labelY, label = stratum),
      inherit.aes = FALSE,
      hjust = stratumLabels$labelhjust,
      size = 3
    ) +
    scale_x_discrete(limits = c("Source pool", "Sink pool")) +
    labs(fill = "Sink Pool Category") +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(panel.grid.major = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          legend.position = c(0.91, 0.15)) +
    scale_fill_manual(values = c(
      "Emission" = "#a4672c",
      "Slow" = "#d6d893",
      "Medium" = "#733958",
      "Fast" = "#5c538a",
      "Very fast" = "#4e88b9",
      "Products" = "#8A9A5B"
    ))

  return(alluvialDist)
}

