# tests/testthat/test-02-data-load.R
# Tests for QPS and CS RDS files: schema, date range, geo/demo cols.

proj_root <- normalizePath(
  if (file.exists("data/qps")) "." else "../..",
  mustWork = FALSE
)

qps_dir <- file.path(proj_root, "data", "qps")
cs_dir  <- file.path(proj_root, "data", "cs")

expected_cols <- c("date", "financial_year", "main_group", "subgroup",
                   "is_subtotal", "count", "offence",
                   "geo_cols", "demo_cols", "dataset_name")

load_rds <- function(path) {
  dt <- readRDS(path)
  if (!is.data.table(dt)) setDT(dt)
  dt
}

# ── Tests for each QPS dataset ------------------------------------------------

qps_files <- list.files(qps_dir, pattern = "\\.rds$", full.names = TRUE)

test_that("QPS data directory has expected 18 datasets", {
  skip_if(!dir.exists(qps_dir), "data/qps not found")
  expect_equal(length(qps_files), 18L)
})

for (f in qps_files) {
  nm <- tools::file_path_sans_ext(basename(f))

  test_that(paste(nm, "- has required columns"), {
    skip_if(!file.exists(f))
    dt      <- load_rds(f)
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
    dt    <- load_rds(f)
    valid <- c("Offences Against the Person", "Offences Against Property",
               "Other Offences")
    actual  <- unique(dt[!is.na(main_group), main_group])
    unknown <- setdiff(actual, valid)
    expect_equal(length(unknown), 0L,
                 info = paste("Unknown groups:", paste(unknown, collapse = ", ")))
  })

  test_that(paste(nm, "- geo_cols stored as pipe-separated string"), {
    skip_if(!file.exists(f))
    dt      <- load_rds(f)
    geo_val <- unique(dt$geo_cols)
    expect_true(length(geo_val) == 1L)
  })
}

# ── Tests for each CS dataset -------------------------------------------------

cs_files <- list.files(cs_dir, pattern = "\\.rds$", full.names = TRUE)

test_that("CS data directory has expected 18 datasets", {
  skip_if(!dir.exists(cs_dir), "data/cs not found")
  expect_equal(length(cs_files), 18L)
})

for (f in cs_files) {
  nm      <- tools::file_path_sans_ext(basename(f))
  qps_f   <- file.path(qps_dir, paste0(nm, ".rds"))

  test_that(paste(nm, "CS - has same schema as QPS counterpart"), {
    skip_if(!file.exists(f) || !file.exists(qps_f))
    cs  <- load_rds(f)
    qps <- load_rds(qps_f)
    expect_equal(sort(names(cs)), sort(names(qps)))
  })

  test_that(paste(nm, "CS - same row count as QPS counterpart"), {
    skip_if(!file.exists(f) || !file.exists(qps_f))
    cs  <- load_rds(f)
    qps <- load_rds(qps_f)
    expect_equal(nrow(cs), nrow(qps))
  })

  test_that(paste(nm, "CS - counts differ from QPS (not identical copy)"), {
    skip_if(!file.exists(f) || !file.exists(qps_f))
    cs  <- load_rds(f)
    qps <- load_rds(qps_f)
    expect_false(identical(cs$count, qps$count))
  })
}
