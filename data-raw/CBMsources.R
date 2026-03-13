
# Read data from CSV
CBMsourceCSVs <- list.files("data-raw/CBMsources", pattern = "\\.csv", full = TRUE)
CBMsources <- data.table::rbindlist(lapply(CBMsourceCSVs, function(csv){
  data.table::fread(csv, na.strings = "")
}), fill = TRUE)

# Check data
for (field in c("provider", "type", "attr", "targetFile", "url")){
  if (any(is.na(CBMsources[[field]]))) stop(shQuote(field), " field has NAs")
}
if (!all(CBMsources$type %in% c("vector", "raster"))) stop("\"type\" must be 'vector' or 'raster'")

# Set source ID
CBMsources[, sourceID := paste(na.omit(c(
  provider, year, region, attr
)), collapse = "-"), by = 1:nrow(CBMsources)]

# Format
CBMsources <- CBMsources[, .(
  provider = unique(provider),
  year     = unique(year),
  region   = unique(region),
  attr     = unique(attr),
  type     = unique(type),
  source   = list(data.table::data.table(
    layer      = layer,
    field      = field,
    subattr    = subattr,
    targetFile = targetFile,
    url        = url
  )),
  notes = ifelse(length(na.omit(notes)) > 0, paste(na.omit(notes), collapse = ";"), NA_character_)
), by = "sourceID"]

data.table::setkey(CBMsources, region, sourceID)

# Save
usethis::use_data(CBMsources, internal = FALSE, overwrite = TRUE)



