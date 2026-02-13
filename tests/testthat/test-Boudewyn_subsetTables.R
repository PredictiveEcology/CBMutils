if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Download Boudewyn tables
boudewynTables <- lapply(3:7, function(n){
  url <- paste0("https://nfi.nfis.org/resources/biomass_models/appendix2_table", n, ".csv")
  destfile <- file.path(testDirs$temp$inputs, basename(url))
  if (!file.exists(destfile)) download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
  data.table::fread(destfile)
})
names(boudewynTables) <- 3:7

# Download CBM defaults database
cbm_defaults_db <- {
  url = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.9300.391.db"
  destfile <- file.path(testDirs$temp$inputs, basename(url))
  if (!file.exists(destfile)) download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
  destfile
}

# Test that all CBM spatial units have Boudewyn parameters
cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db)
spatial_units <- RSQLite::dbReadTable(cbmDBcon, "spatial_unit") |>
  data.table::as.data.table(key = "id")
RSQLite::dbDisconnect(cbmDBcon)

# Set admin abbreviations
spatial_units[, juris_id := c(
  "NL", "NL", "NS", "PE", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YK", "NT", "NU"
)[spatial_units$admin_boundary_id],]

for (i in 1:nrow(spatial_units)){

  testName <- with(spatial_units[i,], sprintf(
    "boudewynSubsetTables: juris_id = %s; ecozone = %s; ", juris_id, eco_boundary_id))

  test_that(testName, {

    thisAdmin <- data.frame(
      juris_id = spatial_units[i,]$juris_id,
      ecozone  = spatial_units[i,]$eco_boundary_id
    )

    expect_is(boudewynSubsetTables(boudewynTables[["3"]], thisAdmin), "data.table")
    expect_is(boudewynSubsetTables(boudewynTables[["4"]], thisAdmin), "data.table")
    expect_is(boudewynSubsetTables(boudewynTables[["5"]], thisAdmin), "data.table")
    expect_is(boudewynSubsetTables(boudewynTables[["6"]], thisAdmin), "data.table")
    expect_is(boudewynSubsetTables(boudewynTables[["7"]], thisAdmin), "data.table")
  })
}


