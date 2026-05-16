# tests/testthat/test-01-hierarchy.R
# Tests for R/hierarchy_map.R: build_hierarchy(), classify_by_pattern()

source(file.path(dirname(dirname(getwd())), "R", "hierarchy_map.R"),
       local = TRUE)

# Resolve path whether run from project root or tests/testthat/
proj_root <- normalizePath(
  if (file.exists("R/hierarchy_map.R")) "." else "../..",
  mustWork = FALSE
)
source(file.path(proj_root, "R", "hierarchy_map.R"), local = TRUE)


test_that("build_hierarchy returns a data.table with required columns", {
  cols <- c("Homicide (Murder)", "Assault", "Drug Offences", "Unlawful Entry")
  hier <- build_hierarchy(cols)

  expect_s3_class(hier, "data.table")
  expect_true(all(c("offence_col", "main_group", "subgroup", "is_subtotal")
                  %in% names(hier)))
  expect_equal(nrow(hier), length(cols))
})

test_that("known offence columns classify to correct main_group", {
  cols <- c("Homicide (Murder)", "Assault", "Drug Offences",
            "Unlawful Entry", "Fraud")
  hier <- build_hierarchy(cols)
  setkey(hier, offence_col)

  expect_equal(hier["Homicide (Murder)", main_group],
               "Offences Against the Person")
  expect_equal(hier["Assault",           main_group],
               "Offences Against the Person")
  expect_equal(hier["Drug Offences",     main_group],
               "Other Offences")
  expect_equal(hier["Unlawful Entry",    main_group],
               "Offences Against Property")
  expect_equal(hier["Fraud",             main_group],
               "Offences Against Property")
})

test_that("known offence columns classify to correct subgroup", {
  cols <- c("Homicide (Murder)", "Drink Driving", "Armed Robbery",
            "Rape and Attempted Rape")
  hier <- build_hierarchy(cols)
  setkey(hier, offence_col)

  expect_equal(hier["Homicide (Murder)",      subgroup], "Homicide")
  expect_equal(hier["Drink Driving",          subgroup], "Traffic Offences")
  expect_equal(hier["Armed Robbery",          subgroup], "Robbery")
  expect_equal(hier["Rape and Attempted Rape", subgroup], "Sexual Offences")
})

test_that("subtotal flags are set correctly", {
  cols <- c("Assault", "Common Assault'", "Drug Offences", "Possess Drugs")
  hier <- build_hierarchy(cols)
  setkey(hier, offence_col)

  expect_true( hier["Assault",       is_subtotal])
  expect_false(hier["Common Assault'", is_subtotal])
  expect_true( hier["Drug Offences", is_subtotal])
  expect_false(hier["Possess Drugs", is_subtotal])
})

test_that("unknown columns get a non-NA classification via pattern fallback", {
  cols <- c("Some Unknown Homicide Type", "Mystery Drug Offence")
  hier <- build_hierarchy(cols)

  expect_equal(nrow(hier), 2L)
  expect_false(any(is.na(hier$main_group)))
})

test_that("build_hierarchy handles empty input gracefully", {
  hier <- build_hierarchy(character(0L))
  expect_s3_class(hier, "data.table")
  expect_equal(nrow(hier), 0L)
})
