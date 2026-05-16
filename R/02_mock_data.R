# R/02_mock_data.R
# Generates a synthetic counterpart for every processed dataset.
#
# Per-offence strategy:
#   1. Draw a random scale factor ~ Uniform(0.70, 1.30) for each individual offence.
#   2. Apply factor to real counts, then add Poisson noise -> mock counts.
#   3. Subtotal columns are recomputed by summing their mock sub-types so the
#      dataset stays internally consistent.
#   4. Structure (dates, geography, demographics, column names) is identical to real.
#
# Output:  data/mock/<name>_mock.rds   (one data.table per processed dataset)
# Run from project root:  source("R/02_mock_data.R")

source("R/hierarchy_map.R")   # also handles package installation

dir.create("data/mock", recursive = TRUE, showWarnings = FALSE)

processed_files <- list.files("data/processed", pattern = "\\.rds$", full.names = TRUE)
if (length(processed_files) == 0L)
  stop("No processed files found. Run 01_download_and_process.R first.")

set.seed(2024L)

# Columns that are part of the dataset identity (not offence counts)
META_COLS <- c("offence", "count", "main_group", "subgroup", "is_subtotal",
               "dataset_name", "geo_cols", "demo_cols",
               "date", "month", "year", "financial_year")


generate_mock <- function(dt_real, name) {

  # ── 1. Draw one scale factor per individual offence ───────────────────────
  individual_offences <- unique(dt_real[is_subtotal == FALSE, offence])
  scale_dt <- data.table(
    offence      = individual_offences,
    scale_factor = runif(length(individual_offences), min = 0.70, max = 1.30)
  )

  # ── 2. Apply scale factor + Poisson noise to individual offence counts ────
  dt_indiv <- copy(dt_real[is_subtotal == FALSE])
  dt_indiv[scale_dt, on = "offence", scale_factor := i.scale_factor]

  dt_indiv[!is.na(count),
           count := {
             lam <- pmax(0.1, count * scale_factor)
             as.numeric(rpois(.N, lambda = lam))
           }]

  dt_indiv[, scale_factor := NULL]

  # ── 3. Recompute subtotal counts from mock individual offences ─────────────
  # Group-by keys for summing: everything except the offence identity & count
  by_cols <- setdiff(names(dt_real), META_COLS)

  # Sum mock individual counts per subgroup × all id variables
  subgroup_sums <- dt_indiv[,
    .(subgroup_sum = sum(count, na.rm = TRUE)),
    by = c(by_cols, "main_group", "subgroup",
           "dataset_name", "geo_cols", "demo_cols",
           "date", "month", "year", "financial_year")
  ]

  # Pull subtotal rows from real data, drop their counts (will be replaced)
  dt_sub_meta <- copy(dt_real[is_subtotal == TRUE])
  dt_sub_meta[, count := NULL]

  join_keys <- c(by_cols, "main_group", "subgroup",
                 "dataset_name", "geo_cols", "demo_cols",
                 "date", "month", "year", "financial_year")

  dt_sub_new <- subgroup_sums[dt_sub_meta, on = join_keys]
  dt_sub_new[, count       := fifelse(is.na(subgroup_sum), 0, subgroup_sum)]
  dt_sub_new[, subgroup_sum := NULL]

  # ── 4. Combine and sort ───────────────────────────────────────────────────
  dt_mock <- rbindlist(list(dt_indiv, dt_sub_new), use.names = TRUE, fill = TRUE)
  setorder(dt_mock, date, offence)
  dt_mock
}


# ── Main loop ──────────────────────────────────────────────────────────────────

mock_list <- list()

for (f in processed_files) {
  name <- tools::file_path_sans_ext(basename(f))
  message("Generating mock for: ", name)

  dt_real <- tryCatch(readRDS(f),
                       error = function(e) { message("  Read ERROR: ", e$message); NULL })
  if (is.null(dt_real)) next
  if (!is.data.table(dt_real)) setDT(dt_real)

  dt_mock <- tryCatch(
    generate_mock(dt_real, name),
    error = function(e) { message("  Generate ERROR: ", e$message); NULL }
  )
  if (is.null(dt_mock)) next

  out_path <- file.path("data/mock", paste0(name, "_mock.rds"))
  saveRDS(dt_mock, out_path)
  message("  Saved -> ", out_path)
  mock_list[[name]] <- dt_mock
}


# ── Validation summary ─────────────────────────────────────────────────────────

cat("\n===== MOCK DATA COMPLETE =====\n")
cat("Generated:", length(mock_list), "mock datasets\n\n")

for (nm in names(mock_list)) {
  real_path <- file.path("data/processed", paste0(nm, ".rds"))
  if (!file.exists(real_path)) next

  dt_r <- readRDS(real_path)[is_subtotal == FALSE]
  dt_m <- mock_list[[nm]][is_subtotal == FALSE]

  real_total <- dt_r[, sum(count, na.rm = TRUE)]
  mock_total <- dt_m[, sum(count, na.rm = TRUE)]
  pct_diff   <- round((mock_total - real_total) / real_total * 100, 1)

  cat(sprintf("%-30s  real: %10.0f  mock: %10.0f  diff: %+.1f%%\n",
              nm, real_total, mock_total, pct_diff))
}

cat("\nNext step: shiny::runApp('app')\n")
