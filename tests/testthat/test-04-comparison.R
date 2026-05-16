# tests/testthat/test-04-comparison.R
# Tests for comp_subgroup and comp_monthly reactives in pair_server.

proj_root <- normalizePath(
  if (file.exists("app/modules/pair_module.R")) "." else "../..",
  mustWork = FALSE
)
source(file.path(proj_root, "app", "global.R"), local = TRUE)

make_pair <- function() {
  list(real = REAL_DT, mock = MOCK_DT)
}

fy_all <- unique(REAL_DT$financial_year)

# ── comp_subgroup tests -------------------------------------------------------

test_that("comp_subgroup has expected columns", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    cs <- comp_subgroup()
    expect_true(all(c("subgroup", "real", "mock", "diff", "pct_diff")
                    %in% names(cs)))
  })
})

test_that("comp_subgroup includes a TOTAL row", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    cs <- comp_subgroup()
    expect_true("TOTAL" %in% cs$subgroup)
  })
})

test_that("comp_subgroup TOTAL equals sum of subgroup rows", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    cs <- comp_subgroup()
    total_row  <- cs[subgroup == "TOTAL"]
    detail_sum <- cs[subgroup != "TOTAL", sum(real)]
    expect_equal(total_row$real, detail_sum)
  })
})

test_that("comp_subgroup diff = mock - real", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    cs <- comp_subgroup()
    expect_equal(cs$diff, cs$mock - cs$real)
  })
})

test_that("comp_subgroup pct_diff is NA when real is 0", {
  real_zero <- copy(REAL_DT)
  real_zero[subgroup == "Assault", count := 0L]
  mock_zero <- copy(MOCK_DT)

  testServer(pair_server,
             args = list(pair_data = list(real = real_zero, mock = mock_zero),
                         metric = "Number"), {
    session$setInputs(
      main_group = "Offences Against the Person",
      fy         = fy_all,
      subgroup   = "Assault"
    )
    cs <- comp_subgroup()
    assault_row <- cs[subgroup == "Assault"]
    if (nrow(assault_row) > 0L && assault_row$real == 0L)
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
    expect_true(all(c("month", "financial_year", "real", "mock", "diff", "pct_diff")
                    %in% names(cm)))
  })
})

test_that("comp_monthly row count equals unique date × FY combos in filtered data", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    cm <- comp_monthly()
    fr <- filt_real()
    expected_n <- uniqueN(fr[, .(date, financial_year)])
    expect_equal(nrow(cm), expected_n)
  })
})

test_that("comp_monthly diff = mock - real for each row", {
  testServer(pair_server, args = list(pair_data = make_pair(), metric = "Number"), {
    session$setInputs(
      main_group = c("Offences Against the Person", "Other Offences"),
      fy         = fy_all,
      subgroup   = "All"
    )
    cm <- comp_monthly()
    expect_equal(cm$diff, cm$mock - cm$real)
  })
})
