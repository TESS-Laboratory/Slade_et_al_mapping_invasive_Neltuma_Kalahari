# Run with: Rscript tests/testthat.R   (or tools/run-tests.sh)
# Pure-function tests for the pipeline modules; no store, no data-in access.
source("tools/testthat-helpers.R")
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
