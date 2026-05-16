setwd("C:/Users/M/Git/qps_app")
source("R/hierarchy_map.R")
library(data.table)
set.seed(2024L)

META_COLS <- c("offence", "count", "main_group", "subgroup", "is_subtotal",
               "dataset_name", "geo_cols", "demo_cols",
               "date", "month", "year", "financial_year")

generate_cs <- function(dt_qps, name) {
  individual_offences <- unique(dt_qps[is_subtotal == FALSE, offence])
  scale_dt <- data.table(
    offence      = individual_offences,
    scale_factor = runif(length(individual_offences), min = 0.70, max = 1.30)
  )
  dt_indiv <- copy(dt_qps[is_subtotal == FALSE])
  dt_indiv[scale_dt, on = "offence", scale_factor := i.scale_factor]
  dt_indiv[!is.na(count),
           count := {
             lam <- pmax(0.1, count * scale_factor)
             as.numeric(rpois(.N, lambda = lam))
           }]
  dt_indiv[, scale_factor := NULL]
  by_cols <- setdiff(names(dt_qps), META_COLS)
  subgroup_sums <- dt_indiv[,
    .(subgroup_sum = sum(count, na.rm = TRUE)),
    by = c(by_cols, "main_group", "subgroup",
           "dataset_name", "geo_cols", "demo_cols",
           "date", "month", "year", "financial_year")]
  dt_sub_meta <- copy(dt_qps[is_subtotal == TRUE])
  dt_sub_meta[, count := NULL]
  join_keys <- c(by_cols, "main_group", "subgroup",
                 "dataset_name", "geo_cols", "demo_cols",
                 "date", "month", "year", "financial_year")
  dt_sub_new <- subgroup_sums[dt_sub_meta, on = join_keys]
  dt_sub_new[, count        := fifelse(is.na(subgroup_sum), 0, subgroup_sum)]
  dt_sub_new[, subgroup_sum := NULL]
  dt_cs <- rbindlist(list(dt_indiv, dt_sub_new), use.names = TRUE, fill = TRUE)
  setorder(dt_cs, date, offence)
  dt_cs
}

nm     <- "district_victims_num"
dt_qps <- readRDS(paste0("data/qps/", nm, ".rds"))
if (!is.data.table(dt_qps)) setDT(dt_qps)
cat("QPS date NAs:", sum(is.na(dt_qps$date)), "\n")
dt_cs <- generate_cs(dt_qps, nm)
saveRDS(dt_cs, paste0("data/cs/", nm, ".rds"))
cat("Saved. CS date NAs:", sum(is.na(dt_cs$date)),
    "| FY sample:", dt_cs$financial_year[1], "\n")
