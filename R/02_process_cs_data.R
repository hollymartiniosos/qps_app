# R/02_process_cs_data.R
# Produces the CS dataset counterpart for every processed QPS dataset.
#
# In production: replace this script with one that reads and processes the
# actual CS source files into data/cs/<name>.rds matching the same schema.
#
# Currently: generates a synthetic CS dataset by applying per-offence scale
# factors and Poisson noise to the QPS data — useful for development and
# testing until real CS data is available.
#
# Per-offence strategy:
#   1. Draw a random scale factor ~ Uniform(0.70, 1.30) per individual offence.
#   2. Apply factor to QPS counts, then add Poisson noise → CS counts.
#   3. Subtotal rows are recomputed by summing their CS sub-types so the
#      dataset stays internally consistent.
#   4. Structure (dates, geography, demographics, column names) is identical
#      to the QPS source.
#
# Input:   data/qps/<name>.rds
# Output:  data/cs/<name>.rds
# Run from project root:  source("R/02_process_cs_data.R")

source("R/hierarchy_map.R")   # also handles package installation

dir.create("data/cs", recursive = TRUE, showWarnings = FALSE)

qps_files <- list.files("data/qps", pattern = "\\.rds$", full.names = TRUE)
if (length(qps_files) == 0L)
  stop("No QPS files found in data/qps/. Run 01_download_and_process.R first.")

set.seed(2024L)

# Columns that are part of the dataset identity (not offence counts)
META_COLS <- c("offence", "count", "main_group", "subgroup", "is_subtotal",
               "dataset_name", "geo_cols", "demo_cols",
               "date", "month", "year", "financial_year")


generate_cs <- function(dt_qps, name) {

  # ── 1. Draw one scale factor per individual offence ───────────────────────
  individual_offences <- unique(dt_qps[is_subtotal == FALSE, offence])
  scale_dt <- data.table(
    offence      = individual_offences,
    scale_factor = runif(length(individual_offences), min = 0.70, max = 1.30)
  )

  # ── 2. Apply scale factor + Poisson noise to individual offence counts ────
  dt_indiv <- copy(dt_qps[is_subtotal == FALSE])
  dt_indiv[scale_dt, on = "offence", scale_factor := i.scale_factor]

  dt_indiv[!is.na(count),
           count := {
             lam <- pmax(0.1, count * scale_factor)
             as.numeric(rpois(.N, lambda = lam))
           }]

  dt_indiv[, scale_factor := NULL]

  # ── 3. Recompute subtotal counts from CS individual offences ──────────────
  by_cols <- setdiff(names(dt_qps), META_COLS)

  subgroup_sums <- dt_indiv[,
    .(subgroup_sum = sum(count, na.rm = TRUE)),
    by = c(by_cols, "main_group", "subgroup",
           "dataset_name", "geo_cols", "demo_cols",
           "date", "month", "year", "financial_year")
  ]

  dt_sub_meta <- copy(dt_qps[is_subtotal == TRUE])
  dt_sub_meta[, count := NULL]

  join_keys <- c(by_cols, "main_group", "subgroup",
                 "dataset_name", "geo_cols", "demo_cols",
                 "date", "month", "year", "financial_year")

  dt_sub_new <- subgroup_sums[dt_sub_meta, on = join_keys]
  dt_sub_new[, count        := fifelse(is.na(subgroup_sum), 0, subgroup_sum)]
  dt_sub_new[, subgroup_sum := NULL]

  # ── 4. Combine and sort ───────────────────────────────────────────────────
  dt_cs <- rbindlist(list(dt_indiv, dt_sub_new), use.names = TRUE, fill = TRUE)
  setorder(dt_cs, date, offence)
  dt_cs
}


# ── Main loop ──────────────────────────────────────────────────────────────────

cs_list <- list()

for (f in qps_files) {
  name <- tools::file_path_sans_ext(basename(f))
  message("Generating CS data for: ", name)

  dt_qps <- tryCatch(readRDS(f),
                     error = function(e) { message("  Read ERROR: ", e$message); NULL })
  if (is.null(dt_qps)) next
  if (!is.data.table(dt_qps)) setDT(dt_qps)

  dt_cs <- tryCatch(
    generate_cs(dt_qps, name),
    error = function(e) { message("  Generate ERROR: ", e$message); NULL }
  )
  if (is.null(dt_cs)) next

  out_path <- file.path("data/cs", paste0(name, ".rds"))
  saveRDS(dt_cs, out_path)
  message("  Saved -> ", out_path)
  cs_list[[name]] <- dt_cs
}


# ── Validation summary ─────────────────────────────────────────────────────────

cat("\n===== CS DATA COMPLETE =====\n")
cat("Generated:", length(cs_list), "CS datasets\n\n")

for (nm in names(cs_list)) {
  qps_path <- file.path("data/qps", paste0(nm, ".rds"))
  if (!file.exists(qps_path)) next

  dt_q <- readRDS(qps_path)[is_subtotal == FALSE]
  dt_c <- cs_list[[nm]][is_subtotal == FALSE]

  qps_total <- dt_q[, sum(count, na.rm = TRUE)]
  cs_total  <- dt_c[, sum(count, na.rm = TRUE)]
  pct_diff  <- round((cs_total - qps_total) / qps_total * 100, 1)

  cat(sprintf("%-30s  QPS: %10.0f  CS: %10.0f  diff: %+.1f%%\n",
              nm, qps_total, cs_total, pct_diff))
}

cat("\nNext step: shiny::runApp('app')\n")
