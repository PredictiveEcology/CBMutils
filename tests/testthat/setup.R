
if (!testthat::is_testing()){
  library(testthat)
  devtools::load_all()
}

# Set up test directories
testDirs <- .testDirectorySetUp()

# Read Boudewyn parameters
bParams <- lapply(
  list(
    table3   = "appendix2_table3.csv",
    table4   = "appendix2_table4.csv",
    table5   = "appendix2_table5.csv",
    table6   = "appendix2_table6.csv",
    table6tb = "appendix2_table6_tb.csv",
    table7   = "appendix2_table7.csv",
    table7tb = "appendix2_table7_tb.csv"
  ),
  function(f) data.table::fread(file.path("https://nfi.nfis.org/resources/biomass_models", f))
)
