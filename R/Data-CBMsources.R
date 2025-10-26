
#' CBM Data Sources
#'
#' Data sources for CBM simulations. These can be read with `CBMsourcePrepInputs`.
#'
#' @format A data table with `r ncol(CBMsources)` columns and `r nrow(CBMsources)` rows:
#' \describe{
#'   \item{sourceID}{Source identifier.}
#'   \item{provider}{Data provider or inventory identifier.}
#'   \item{year}{Data year. The year that the data represents or was published.}
#'   \item{region}{Spatial domain. Defaults to all of Canada.}
#'   \item{attr}{Cohort or stand attribute defined by the source.}
#'   \item{type}{Spatial data type: 'vector' or 'raster'.}
#'   \item{source}{Data table of one or more downloadable data sources:
#'     \itemize{
#'       \item url: Download URL of target data source.
#'       \item targetFile: File name of target data source.
#'       \item layer: Vector or raster layer.
#'       \item field: Vector attribute field.
#'       \item subattr: If the source is comprised of multiple vector fields or raster layers,
#'       this secondary attribute distinguishes each one.
#'     }
#'   }
#'   ...
#' }
"CBMsources"
utils::globalVariables("CBMsources")

#' CBM source: Prep inputs
#'
#' Prepare a data source from the \code{\link{CBMsources}} table
#' with \code{link[reproducible]{prepInputs}}.
#'
#' @param sourceID Source identifier.
#' @param inputPath character.
#' Path of download destination directory.
#' Downloads will be sorted into subdirectories by data provider and region.
#' @param ... additional arguments to \code{link[reproducible]{prepInputs}}
#'
#' @export
CBMsourcePrepInputs <- function(sourceID,
                                inputPath = getOption("spades.inputPath", "."),
                                ...){

  if (!requireNamespace("reproducible", quietly = TRUE)) stop(
    "The package \"reproducible\" is required")

  if (is.null(inputPath)) stop("inputPath missing")

  if (!sourceID %in% CBMsources$sourceID) stop("Invalid sourceID. See the `CBMsources` table")
  sourceIDsel <- sourceID
  srcInfo  <- as.list(CBMsources[sourceID == sourceIDsel,][, source := NULL])
  srcItems <- CBMsources[sourceID == sourceIDsel,]$source[[1]]

  # Download source(s) from URL
  srcInfo$destinationPath <- file.path(
    inputPath, paste(na.omit(c(srcInfo$provider, srcInfo$year, srcInfo$region)), collapse = "-"))

  srcUq <- unique(srcItems[, .SD, .SDcols = c("targetFile", "url")])
  srcUq$path <- sapply(1:nrow(srcUq), function(i){

    reproducible::prepInputs(
      destinationPath = srcInfo$destinationPath,
      url             = srcUq[i,]$url,
      targetFile      = srcUq[i,]$targetFile,
      filename1       = if (tools::file_ext(srcUq[i,]$url) == "zip" &
                            tools::file_ext(srcUq[i,]$targetFile) != "zip") basename(srcUq[i,]$url),
      archive         = if (tools::file_ext(srcUq[i,]$targetFile) %in% c("zip", "tar", "rar")) NA,
      alsoExtract     = "similar",
      fun             = NA,
      useCache        = FALSE,
      ...) |> as.character()
  })

  # Read source
  srcItems <- merge(srcItems, srcUq, by = c("targetFile", "url"), sort = FALSE)
  srcItems$name <- if (nrow(srcItems) == 1) srcInfo$attr else srcItems$subattr
  if (srcInfo$type == "vector"){

    if (length(unique(srcItems$path))  > 1) stop("Vector source must be a single file")
    if (length(unique(srcItems$layer)) > 1) stop("Vector source must be a single layer")

    if (any(is.na(srcItems$field))) stop("Vector source must have \"field\" to extract")
    if (any(is.na(srcItems$layer))){
      layer <- sf::st_layers(srcUq$path)$name
      if (length(layer) != 1) stop("Vector source must have \"layer\" set to one of: ",
                                   paste(sQuote(layer), collapse = ", "))
      srcItems$layer <- layer
    }

    srcInfo$source <- sf::st_read(
      srcUq$path,
      query = paste(c(
        "SELECT",
        paste(
          paste(srcItems$field, "AS", srcItems$name),
          collapse = ", "),
        "FROM",
        srcItems$layer[[1]],
        if (nrow(srcItems) == 1) paste("WHERE", srcItems$field, "IS NOT NULL")
      ), collapse = " "),
      agr   = "constant",
      quiet = TRUE)


  }else if (srcInfo$type == "raster"){

    srcInfo$source <- do.call(c, lapply(1:nrow(srcItems), function(i){
      lyr <- terra::rast(
        srcItems[i,]$path,
        lyrs = ifelse(!is.na(srcItems[i,]$layer), as.numeric(srcItems[i,]$layer), 1))
      names(lyr) <- srcItems[i,]$name
      lyr
    }))

  }else stop("\"type\" must be 'vector' or 'raster'")

  return(srcInfo)
}

#' CBM source: Extract to raster
#'
#' Prepare a data source from the \code{\link{CBMsources}} table
#' with \code{link[reproducible]{prepInputs}},
#' then align the data with a template raster and extract values for each cell
#' with \code{link{extractToRast}}.
#'
#' @param sourceID Source identifier.
#' @param templateRast SpatRaster. Raster template.
#' @param returnSource logical. Return the source spatial data object.
#' @param ... additional arguments to \code{link[reproducible]{prepInputs}}
#'
#' @export
CBMsourceExtractToRast <- function(sourceID, templateRast, returnSource = FALSE, ...){

  # Read source
  srcCBM <- CBMsourcePrepInputs(sourceID, ...)

  # Apply custom transformations
  crop <- TRUE

  if (sourceID == "SCANFI-2020-LandR"){

    # Choose a leading species from layers with % coverage
    srcCBM$source <- terra::crop(
      srcCBM$source,
      terra::project(terra::as.polygons(templateRast, extent = TRUE), terra::crs(srcCBM$source)),
      snap = "out")
    crop <- FALSE

    inSpp <- data.frame(id = 1:terra::nlyr(srcCBM$source), value = names(srcCBM$source))
    srcCBM$source[srcCBM$source == 0] <- NA
    srcCBM$source <- terra::which.max(srcCBM$source)

    levels(srcCBM$source) <- inSpp
  }

  if (sourceID == "StatCan-admin"){

    # Split Newfoundland and Labrador
    adminSF <- cbind(name = srcCBM$source$admin, srcCBM$source)
    adminSF <- terra::split(
      terra::vect(adminSF),
      terra::vect(sf::st_sfc(sf::st_linestring(rbind(c(8476500, 2297500), c(8565300, 2451300))),
                             crs = sf::st_crs(adminSF))))
    adminSF <- sf::st_as_sf(adminSF, agr = "constant")

    nl_cd <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(
      adminSF[adminSF$name == "Newfoundland and Labrador",])))
    adminSF[adminSF$name == "Newfoundland and Labrador", "name"] <- sapply(
      nl_cd[, "X"] == min(nl_cd[, "X"]), ifelse, "Labrador", "Newfoundland")

    srcCBM$source <- adminSF
  }

  # Extract to raster
  srcCBM$extractToRast <- extractToRast(srcCBM$source, templateRast, crop = crop)
  if (!returnSource) srcCBM$source <- NULL

  if (sourceID == "SCANFI-2020-age"){

    # Ages that are 0 should be NA
    srcCBM$extractToRast[srcCBM$extractToRast == 0] <- NA
  }

  srcCBM
}

