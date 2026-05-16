# tests/testthat/test-04-comparison.R
# Tests for comp_subgroup and comp_monthly reactives in pair_server.

proj_root <- normalizePath(
  if (file.exists("app/modules/pair_module.R")) "." else "../..",
  mustWork = FALSE
)
source(file.path(proj_root, "app", "global.R"), local = TRUE)

make_pair <- function() list(qps = QPS_DT, cs = CS_DT)

fy_all <- unique(QPS_DT$financial_year)

# ── comp_subgroup tests -------------------------------------------------------

test_that("comp_subgroup has expected columns", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    tbl <- comp_subgroup()
    expect_true(all(c("subgroup", "qps", "cs", "diff", "pct_diff") %in% names(tbl)))
  })
})

test_that("comp_subgroup includes a TOTAL row", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    tbl <- comp_subgroup()
    expect_true("TOTAL" %in% tbl$subgroup)
  })
})

test_that("comp_subgroup TOTAL equals sum of subgroup rows", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    tbl        <- comp_subgroup()
    total_row  <- tbl[subgroup == "TOTAL"]
    detail_sum <- tbl[subgroup != "TOTAL", sum(qps)]
    expect_equal(total_row$qps, detail_sum)
  })
})

test_that("comp_subgroup diff = cs - qps", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    tbl <- comp_subgroup()
    expect_equal(tbl$diff, tbl$cs - tbl$qps)
  })
})

test_that("comp_subgroup pct_diff is NA when qps is 0", {
  qps_zero <- copy(QPS_DT)
  qps_zero[subgroup == "Assault", count := 0L]
  cs_zero  <- copy(CS_DT)

  testServer(pair_server,
             args = list(pair_data = list(qps = qps_zero, cs = cs_zero),
                         metric = "Number"), {
    session$setInputs(
      main_group = "Offences Against the Person",
      fy         = fy_all,
      subgroup   = "Assault"
    )
    tbl         <- comp_subgroup()
    assault_row <- tbl[subgroup == "Assault"]
    if (nrow(assault_row) > 0L && assault_row$qps == 0L)
      expect_true(is.na(assault_row$pct_diff))
  })
})

# ── comp_monthly tests --------------------------------------------------------

test_that("comp_monthly has expected columns", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    cm <- comp_monthly()
    expect_true(all(c("month", "financial_year", "qps", "cs", "diff", "pct_diff")
                    %in% names(cm)))
  })
})

test_that("comp_monthly row count equals unique date x FY combos", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    cm         <- comp_monthly()
    fq         <- filt_qps()
    expected_n <- uniqueN(fq[, .(date, financial_year)])
    expect_equal(nrow(cm), expected_n)
  })
})

test_that("comp_monthly diff = cs - qps for each row", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    cm <- comp_monthly()
    expect_equal(cm$diff, cm$cs - cm$qps)
  })
})
