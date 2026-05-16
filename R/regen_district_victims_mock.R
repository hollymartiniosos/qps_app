setwd("C:/Users/M/Git/qps_app")
source("R/hierarchy_map.R")
library(data.table)
set.seed(2024L)

META_COLS <- c("offence", "count", "main_group", "subgroup", "is_subtotal",
               "dataset_name", "geo_cols", "demo_cols",
               "date", "month", "year", "financial_year")

generate_mock <- function(dt_real, name) {
  individual_offences <- unique(dt_real[is_subtotal == FALSE, offence])
  scale_dt <- data.table(
    offence      = individual_offences,
    scale_factor = runif(length(individual_offences), min = 0.70, max = 1.30)
  )
  dt_indiv <- copy(dt_real[is_subtotal == FALSE])
  dt_indiv[scale_dt, on = "offence", scale_factor := i.scale_factor]
  dt_indiv[!is.na(count),
           count := {
             lam <- pmax(0.1, count * scale_factor)
             as.numeric(rpois(.N, lambda = lam))
           }]
  dt_indiv[, scale_factor := NULL]
  by_cols <- setdiff(names(dt_real), META_COLS)
  subgroup_sums <- dt_indiv[,
    .(subgroup_sum = sum(count, na.rm = TRUE)),
    by = c(by_cols, "main_group", "subgroup",
           "dataset_name", "geo_cols", "demo_cols",
           "date", "month", "year", "financial_year")]
  dt_sub_meta <- copy(dt_real[is_subtotal == TRUE])
  dt_sub_meta[, count := NULL]
  join_keys <- c(by_cols, "main_group", "subgroup",
                 "dataset_name", "geo_cols", "demo_cols",
                 "date", "month", "year", "financial_year")
  dt_sub_new <- subgroup_sums[dt_sub_meta, on = join_keys]
  dt_sub_new[, count        := fifelse(is.na(subgroup_sum), 0, subgroup_sum)]
  dt_sub_new[, subgroup_sum := NULL]
  dt_mock <- rbindlist(list(dt_indiv, dt_sub_new), use.names = TRUE, fill = TRUE)
  setorder(dt_mock, date, offence)
  dt_mock
}

nm      <- "district_victims_num"
dt_real <- readRDS(paste0("data/processed/", nm, ".rds"))
if (!is.data.table(dt_real)) setDT(dt_real)
cat("Real date NAs:", sum(is.na(dt_real$date)), "\n")
dt_mock <- generate_mock(dt_real, nm)
saveRDS(dt_mock, paste0("data/mock/", nm, "_mock.rds"))
cat("Saved. Mock date NAs:", sum(is.na(dt_mock$date)),
    "| FY sample:", dt_mock$financial_year[1], "\n")
