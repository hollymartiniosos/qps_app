# tests/testthat/test-03-filter.R
# Tests for pair_server filter logic (filter_dt) using testServer().

proj_root <- normalizePath(
  if (file.exists("app/modules/pair_module.R")) "." else "../..",
  mustWork = FALSE
)
source(file.path(proj_root, "app", "global.R"), local = TRUE)

test_that("filter_dt: no filters returns only non-subtotal rows", {
  pair_data <- list(qps = QPS_DT, cs = CS_DT)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = unique(QPS_DT$financial_year),
      subgroup   = "All"
    )
    r <- filt_qps()
    expect_false(any(r$is_subtotal))
  })
})

test_that("filter_dt: financial year filter restricts rows correctly", {
  pair_data <- list(qps = QPS_DT, cs = CS_DT)
  fy_vals   <- unique(QPS_DT$financial_year)
  target_fy <- fy_vals[1L]

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = target_fy,
      subgroup   = "All"
    )
    r <- filt_qps()
    expect_true(all(r$financial_year == target_fy))
  })
})

test_that("filter_dt: main_group filter restricts rows correctly", {
  pair_data <- list(qps = QPS_DT, cs = CS_DT)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = "Offences Against the Person",
      fy         = unique(QPS_DT$financial_year),
      subgroup   = "All"
    )
    r <- filt_qps()
    expect_true(all(r$main_group == "Offences Against the Person"))
    expect_false("Other Offences" %in% r$main_group)
  })
})

test_that("filter_dt: subgroup filter restricts rows correctly", {
  pair_data <- list(qps = QPS_DT, cs = CS_DT)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = unique(QPS_DT$financial_year),
      subgroup   = "Assault"
    )
    r <- filt_qps()
    expect_true(all(r$subgroup == "Assault"))
  })
})

test_that("filter_dt: empty main_group returns zero rows", {
  pair_data <- list(qps = QPS_DT, cs = CS_DT)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = character(0L),
      fy         = unique(QPS_DT$financial_year),
      subgroup   = "All"
    )
    r <- filt_qps()
    expect_equal(nrow(r), 0L)
  })
})

test_that("filter_dt: geo filter works for datasets with geography", {
  qps_geo <- make_dt(geo = "Region")
  cs_geo  <- copy(qps_geo)
  cs_geo[, count := as.integer(count * 1.1)]
  pair_data <- list(qps = qps_geo, cs = cs_geo)

  testServer(pair_server, args = list(pair_data = pair_data, metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = unique(qps_geo$financial_year),
      subgroup   = "All",
      f_Region   = "North"
    )
    r <- filt_qps()
    expect_true(all(r$Region == "North"))
  })
})
