setwd("C:/Users/M/Git/qps_app")
library(data.table)
for (nm in c("qld_victims_num", "region_victims_num",
             "district_victims_num", "lga_victims_num")) {
  m <- readRDS(paste0("data/mock/", nm, "_mock.rds"))
  cat(nm, "- date NAs:", sum(is.na(m$date)),
      "| FY:", m$financial_year[1], "\n")
}
