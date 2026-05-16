# tests/testthat/test-03-filter.R
# Tests for pair_server filter logic (filter_dt) using testServer().

proj_root <- normalizePath(
  if (file.exists("app/modules/pair_module.R")) "." else "../..",
  mustWork = FALSE
)
source(file.path(proj_root, "app", "global.R"),    local = TRUE)

test_that("filter_dt: no filters returns only non-subtotal rows", {
  pair_data <- list(real = REAL_DT, mock = MOCK_DT)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = unique(REAL_DT$financial_year),
      subgroup   = "All"
    )
    r <- filt_real()
    expect_false(any(r$is_subtotal))
  })
})

test_that("filter_dt: financial year filter restricts rows correctly", {
  pair_data <- list(real = REAL_DT, mock = MOCK_DT)
  fy_vals   <- unique(REAL_DT$financial_year)
  target_fy <- fy_vals[1L]

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = target_fy,
      subgroup   = "All"
    )
    r <- filt_real()
    expect_true(all(r$financial_year == target_fy))
  })
})

test_that("filter_dt: main_group filter restricts rows correctly", {
  pair_data <- list(real = REAL_DT, mock = MOCK_DT)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = "Offences Against the Person",
      fy         = unique(REAL_DT$financial_year),
      subgroup   = "All"
    )
    r <- filt_real()
    expect_true(all(r$main_group == "Offences Against the Person"))
    expect_false("Other Offences" %in% r$main_group)
  })
})

test_that("filter_dt: subgroup filter restricts rows correctly", {
  pair_data <- list(real = REAL_DT, mock = MOCK_DT)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = unique(REAL_DT$financial_year),
      subgroup   = "Assault"
    )
    r <- filt_real()
    expect_true(all(r$subgroup == "Assault"))
  })
})

test_that("filter_dt: empty main_group returns zero rows", {
  pair_data <- list(real = REAL_DT, mock = MOCK_DT)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = character(0L),
      fy         = unique(REAL_DT$financial_year),
      subgroup   = "All"
    )
    r <- filt_real()
    expect_equal(nrow(r), 0L)
  })
})

test_that("filter_dt: geo filter works for datasets with geography", {
  real_geo <- make_dt(geo = "Region")
  mock_geo <- copy(real_geo)
  mock_geo[, count := as.integer(count * 1.1)]
  pair_data <- list(real = real_geo, mock = mock_geo)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = unique(real_geo$financial_year),
      subgroup   = "All",
      f_Region   = "North"
    )
    r <- filt_real()
    expect_true(all(r$Region == "North"))
  })
})
