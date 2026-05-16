# tests/run_tests.R
# Run the full test suite from the project root and write a test log.
# Usage:  Rscript tests/run_tests.R

if (!requireNamespace("testthat", quietly = TRUE))
  install.packages("testthat", repos = "https://cloud.r-project.org")

library(testthat)

# Always run from project root so relative paths resolve correctly
proj_root <- normalizePath(
  if (file.exists("app/global.R")) "." else "..",
  mustWork = TRUE
)
setwd(proj_root)

log_path <- file.path(proj_root, "tests", "test_log.txt")
sink(log_path)

cat("QPS App – Test Log\n")
cat("==================\n")
cat("Run time :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R version :", R.version$version.string, "\n\n")

results <- tryCatch(
  test_dir(
    file.path(proj_root, "tests", "testthat"),
    reporter   = "progress",
    stop_on_failure = FALSE
  ),
  error = function(e) {
    cat("FATAL ERROR during test_dir():\n", conditionMessage(e), "\n")
    NULL
  }
)

sink()

# Also echo to console
cat(readLines(log_path), sep = "\n")

if (!is.null(results)) {
  n_fail <- sum(as.data.frame(results)$failed)
  n_pass <- sum(as.data.frame(results)$passed)
  cat(sprintf("\n%d passed, %d failed\n", n_pass, n_fail))
  if (n_fail > 0L) quit(status = 1L)
}
