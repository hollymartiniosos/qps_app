setwd("C:/Users/M/Git/qps_app")
library(data.table)
for (nm in c("qld_victims_num", "region_victims_num",
             "district_victims_num", "lga_victims_num")) {
  cs <- readRDS(paste0("data/cs/", nm, ".rds"))
  cat(nm, "- date NAs:", sum(is.na(cs$date)),
      "| FY:", cs$financial_year[1], "\n")
}
