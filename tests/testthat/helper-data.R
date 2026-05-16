# tests/testthat/helper-data.R
# Shared test fixtures loaded automatically by testthat before each test file.

library(data.table)
library(shiny)

# ── Minimal processed data.table (mimics real/mock structure) ─────────────────

make_dt <- function(geo = NULL, demo = NULL, n_months = 6L) {
  dates <- seq(as.Date("2023-07-01"), by = "month", length.out = n_months)
  subgroups <- c("Assault", "Homicide", "Drug Offences")

  rows <- CJ(date = dates, subgroup = subgroups)
  rows[, `:=`(
    offence      = paste0(subgroup, " - detail"),
    main_group   = fifelse(subgroup %in% c("Assault", "Homicide"),
                           "Offences Against the Person", "Other Offences"),
    is_subtotal  = FALSE,
    count        = as.integer(round(runif(.N, 10, 100))),
    month        = month(date),
    year         = year(date),
    financial_year = fifelse(month(date) >= 7L,
                             paste0(year(date), "-", year(date) + 1L),
                             paste0(year(date) - 1L, "-", year(date))),
    dataset_name = "test_ds",
    geo_cols     = if (!is.null(geo)) geo else "",
    demo_cols    = if (!is.null(demo)) demo else ""
  )]

  if (!is.null(geo)) {
    geo_col <- strsplit(geo, "\\|")[[1L]][1L]
    rows[, (geo_col) := rep_len(c("North", "South"), .N)]
  }
  if (!is.null(demo)) {
    demo_col <- strsplit(demo, "\\|")[[1L]][1L]
    rows[, (demo_col) := rep_len(c("Male", "Female"), .N)]
  }

  rows
}

REAL_DT <- make_dt()
MOCK_DT <- copy(REAL_DT)
MOCK_DT[, count := as.integer(round(count * runif(.N, 0.85, 1.15)))]
