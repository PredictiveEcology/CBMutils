if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Download CBM defaults database
cbm_defaults_db <- {
  url = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.9300.391.db"
  destfile <- file.path(testDirs$temp$inputs, basename(url))
  if (!file.exists(destfile)) download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
  destfile
}

# Download Boudewyn tables
boudewynTables <- lapply(3:7, function(n){
  url <- paste0("https://nfi.nfis.org/resources/biomass_models/appendix2_table", n, ".csv")
  destfile <- file.path(testDirs$temp$inputs, basename(url))
  if (!file.exists(destfile)) download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
  data.table::fread(destfile)
})
names(boudewynTables) <- 3:7


cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db)
spatial_units <- RSQLite::dbReadTable(cbmDBcon, "spatial_unit") |>
  data.table::as.data.table()
RSQLite::dbDisconnect(cbmDBcon)

spatial_units[, AdminBoundaryID := admin_boundary_id]
spatial_units[, EcoBoundaryID   := eco_boundary_id]
spatial_units[, abreviation     := c(
  "NL", "NL", "NS", "PE", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YK", "NT", "NU"
)[spatial_units$admin_boundary_id],]

data.table::setkey(spatial_units, admin_boundary_id, eco_boundary_id)

for (i in 1:nrow(spatial_units)){

  testName <- with(spatial_units[i,], sprintf(
    "boudewynSubsetTables: admin_boundary_id = %s; eco_boundary_id = %s; spatial_unit_id = %s; ",
    admin_boundary_id, eco_boundary_id, id))

  test_that(testName, {

    thisAdmin <- spatial_units[i,]
    eco <- thisAdmin$EcoBoundaryID

    expect_is(boudewynSubsetTables(table = boudewynTables[["3"]], thisAdmin = thisAdmin, eco = eco), "data.table")
    expect_is(boudewynSubsetTables(table = boudewynTables[["4"]], thisAdmin = thisAdmin, eco = eco), "data.table")
    expect_is(boudewynSubsetTables(table = boudewynTables[["5"]], thisAdmin = thisAdmin, eco = eco), "data.table")
    expect_is(boudewynSubsetTables(table = boudewynTables[["6"]], thisAdmin = thisAdmin, eco = eco), "data.table")
    expect_is(boudewynSubsetTables(table = boudewynTables[["7"]], thisAdmin = thisAdmin, eco = eco), "data.table")
  })
}


