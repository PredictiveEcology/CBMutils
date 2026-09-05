if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Download CBM-CFS3 database
dbPath <- {
  url = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db"
  destfile <- file.path(testDirs$temp$inputs, basename(url))
  if (!file.exists(destfile)) download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
  destfile
}

cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), dbPath)
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

    expect_is(boudewynSubsetTables(table = bParams$table3, thisAdmin = thisAdmin, eco = eco), "data.table")
    expect_is(boudewynSubsetTables(table = bParams$table4, thisAdmin = thisAdmin, eco = eco), "data.table")
    expect_is(boudewynSubsetTables(table = bParams$table5, thisAdmin = thisAdmin, eco = eco), "data.table")
    expect_is(boudewynSubsetTables(table = bParams$table6, thisAdmin = thisAdmin, eco = eco), "data.table")
    expect_is(boudewynSubsetTables(table = bParams$table7, thisAdmin = thisAdmin, eco = eco), "data.table")
  })
}


