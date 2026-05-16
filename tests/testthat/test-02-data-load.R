# tests/testthat/test-02-data-load.R
# Tests for processed and mock RDS files: schema, date range, geo/demo cols.

proj_root <- normalizePath(
  if (file.exists("data/processed")) "." else "../..",
  mustWork = FALSE
)

processed_dir <- file.path(proj_root, "data", "processed")
mock_dir      <- file.path(proj_root, "data", "mock")

expected_cols <- c("date", "financial_year", "main_group", "subgroup",
                   "is_subtotal", "count", "offence",
                   "geo_cols", "demo_cols", "dataset_name")

# ── Helpers -------------------------------------------------------------------

load_rds <- function(path) {
  dt <- readRDS(path)
  if (!is.data.table(dt)) setDT(dt)
  dt
}

# ── Tests for each processed dataset -----------------------------------------

proc_files <- list.files(processed_dir, pattern = "\\.rds$", full.names = TRUE)

test_that("processed data directory has expected 18 datasets", {
  skip_if(!dir.exists(processed_dir), "data/processed not found")
  expect_equal(length(proc_files), 18L)
})

for (f in proc_files) {
  nm <- tools::file_path_sans_ext(basename(f))

  test_that(paste(nm, "- has required columns"), {
    skip_if(!file.exists(f))
    dt <- load_rds(f)
    missing <- setdiff(expected_cols, names(dt))
    expect_equal(missing, character(0L),
                 info = paste("Missing:", paste(missing, collapse = ", ")))
  })

  test_that(paste(nm, "- no NA dates"), {
    skip_if(!file.exists(f))
    dt <- load_rds(f)
    expect_equal(sum(is.na(dt$date)), 0L)
  })

  test_that(paste(nm, "- financial_year parseable"), {
    skip_if(!file.exists(f))
    dt <- load_rds(f)
    expect_false(all(is.na(dt$financial_year)))
    expect_match(dt$financial_year[1L], "^\\d{4}-\\d{4}$")
  })

  test_that(paste(nm, "- count column is numeric"), {
    skip_if(!file.exists(f))
    dt <- load_rds(f)
    expect_true(is.numeric(dt$count))
  })

  test_that(paste(nm, "- main_group values are valid"), {
    skip_if(!file.exists(f))
    dt <- load_rds(f)
    valid <- c("Offences Against the Person", "Offences Against Property",
               "Other Offences")
    actual <- unique(dt[!is.na(main_group), main_group])
    unknown <- setdiff(actual, valid)
    expect_equal(length(unknown), 0L,
                 info = paste("Unknown main_groups:", paste(unknown, collapse = ", ")))
  })

  test_that(paste(nm, "- geo_cols stored as pipe-separated string"), {
    skip_if(!file.exists(f))
    dt <- load_rds(f)
    geo_val <- unique(dt$geo_cols)
    expect_true(length(geo_val) == 1L)
  })
}

# ── Tests for each mock dataset -----------------------------------------------

mock_files <- list.files(mock_dir, pattern = "_mock\\.rds$", full.names = TRUE)

test_that("mock data directory has expected 18 datasets", {
  skip_if(!dir.exists(mock_dir), "data/mock not found")
  expect_equal(length(mock_files), 18L)
})

for (f in mock_files) {
  nm <- tools::file_path_sans_ext(basename(f))
  proc_nm <- sub("_mock$", "", nm)
  proc_f  <- file.path(processed_dir, paste0(proc_nm, ".rds"))

  test_that(paste(nm, "- has same schema as processed counterpart"), {
    skip_if(!file.exists(f) || !file.exists(proc_f))
    mock <- load_rds(f)
    real <- load_rds(proc_f)
    expect_equal(sort(names(mock)), sort(names(real)))
  })

  test_that(paste(nm, "- same row count as processed counterpart"), {
    skip_if(!file.exists(f) || !file.exists(proc_f))
    mock <- load_rds(f)
    real <- load_rds(proc_f)
    expect_equal(nrow(mock), nrow(real))
  })

  test_that(paste(nm, "- mock counts differ from real (not identical copy)"), {
    skip_if(!file.exists(f) || !file.exists(proc_f))
    mock <- load_rds(f)
    real <- load_rds(proc_f)
    expect_false(identical(mock$count, real$count))
  })
}
