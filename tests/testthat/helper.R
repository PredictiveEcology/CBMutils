
#' Set up test directories
#'
#' List test data directories and set up a temporary directory structure
#' that will be removed on test teardown.
#'
#' @param testPaths file or directory paths within the \code{tests/testthat}
#' directory to add to the file list.
#' By default a test data directory is included with location \code{tests/testthat/testdata}.
#' @param tempDir character. Optional. Path to location of temporary test directory.
#' @param teardownEnv environment. Optional. Environment to use for scoping.
#' The default for testing is the \code{testthat::teardown_env()}.
.testDirectorySetUp <- function(
    testPaths   = "testdata",
    tempDir     = tempdir(),
    teardownEnv = if (testthat::is_testing()) testthat::teardown_env()){

  # Get a list of test directory paths
  testDirs <- .test_directories(testPaths = testPaths, tempDir = tempDir)

  # Create temporary directories
  dir.create(testDirs$temp$root, recursive = TRUE)
  for (d in testDirs$temp) dir.create(d, showWarnings = FALSE)

  # Create temporary directories
  if (!is.null(teardownEnv)){
    withr::defer({
      unlink(testDirs$temp$root, recursive = TRUE)
      if (file.exists(testDirs$temp$root)) warning(
        "Temporary test directory could not be removed: ",
        testDirs$temp$root, call. = FALSE)
    }, envir = teardownEnv, priority = "last")
  }

  # Return test directory paths
  testDirs
}

#' Test directory paths
#'
#' Get a list of test directory paths.
#'
#' @param testPaths file or directory paths within the \code{tests/testthat}
#' directory to add to the file list.
#' @param tempDir character. Optional. Path to location of temporary test directory.
.test_directories <- function(testPaths = NULL, tempDir = tempdir()){

  testDirs <- list()

  # Set custom test paths
  for (testPath in testPaths){
    testDirs[[testPath]] <- testthat::test_path(testPath)
  }

  # Set temporary directory paths
  testPackage <- ifelse(testthat::is_testing(), testthat::testing_package(), basename(getwd()))
  testDirs$temp <- list(
    root = file.path(tempDir, paste0("testthat-", testPackage))
  )
  testDirs$temp$inputs   <- file.path(testDirs$temp$root, "inputs")
  testDirs$temp$outputs  <- file.path(testDirs$temp$root, "outputs")

  # Return
  testDirs
}

