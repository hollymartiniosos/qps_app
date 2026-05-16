setwd("C:/Users/M/Git/qps_app")

source("R/hierarchy_map.R")
library(readxl)
library(data.table)
library(lubridate)

GEO_COLS  <- c("Region", "District", "LGA Name", "LGA", "Division",
               "Police Region", "Police District", "Police Division")
DEMO_COLS <- c("Age", "Sex", "Gender",
               "Offender Age", "Offender Sex",
               "Victim Age",   "Victim Sex")
DATE_COLS <- c("Month Year", "MonthYear", "Month_Year", "Date", "Period")

parse_month_year <- function(x) {
  x_str  <- trimws(as.character(x))
  parsed <- lubridate::parse_date_time(
    x_str,
    orders = c("Ymd", "ymd", "my", "mY", "b y", "b Y", "b-y", "b-Y", "Y-m", "m/Y"),
    locale = "en_US",
    quiet  = TRUE
  )
  still_na <- is.na(parsed)
  if (any(still_na)) {
    parsed[still_na] <- lubridate::parse_date_time(
      paste0("01 ", x_str[still_na]),
      orders = c("d b y", "d b Y"),
      locale = "en_US",
      quiet  = TRUE
    )
  }
  as.Date(parsed)
}

detect_id_cols <- function(dt) {
  cols     <- names(dt)
  date_col <- intersect(cols, DATE_COLS)[1L]
  if (is.na(date_col)) date_col <- cols[1L]
  geo_cols  <- intersect(cols, GEO_COLS)
  demo_cols <- intersect(cols, DEMO_COLS)
  list(date_col = date_col, geo_cols = geo_cols, demo_cols = demo_cols,
       all_id = unique(c(date_col, geo_cols, demo_cols)))
}

process_dataset <- function(dt, name) {
  if (!is.data.table(dt)) setDT(dt)
  id_info <- detect_id_cols(dt)
  offence_cols <- setdiff(names(dt), id_info$all_id)
  is_numeric_col <- function(col) {
    vals <- dt[[col]][!is.na(dt[[col]])]
    if (length(vals) == 0L) return(FALSE)
    suppressWarnings(mean(!is.na(as.numeric(as.character(vals)))) > 0.5)
  }
  offence_cols <- offence_cols[vapply(offence_cols, is_numeric_col, logical(1L))]
  if (length(offence_cols) == 0L) stop("No numeric offence columns found.")
  hier <- build_hierarchy(offence_cols)
  dt_sub  <- dt[, .SD, .SDcols = c(id_info$all_id, offence_cols)]
  dt_long <- melt(dt_sub, id.vars = id_info$all_id, measure.vars = offence_cols,
                  variable.name = "offence", value.name = "count",
                  variable.factor = FALSE)
  dt_long[, count := suppressWarnings(as.numeric(gsub(",", "", as.character(count))))]
  setkey(hier, offence_col)
  dt_long[hier, on = c(offence = "offence_col"),
          `:=`(main_group = i.main_group, subgroup = i.subgroup,
               is_subtotal = i.is_subtotal)]
  dt_long[, date           := parse_month_year(get(id_info$date_col))]
  dt_long[, month          := month(date)]
  dt_long[, year           := year(date)]
  dt_long[, financial_year := fifelse(
    month >= 7L,
    paste0(year, "-", year + 1L),
    paste0(year - 1L, "-", year)
  )]
  dt_long[, dataset_name := name]
  dt_long[, geo_cols     := paste(id_info$geo_cols,  collapse = "|")]
  dt_long[, demo_cols    := paste(id_info$demo_cols, collapse = "|")]
  dt_long
}

BASE_XLSX <- "https://open-crime-data.s3.ap-southeast-2.amazonaws.com/Crime%20Statistics/"
victims <- data.table(
  name = c("qld_victims_num", "region_victims_num",
           "district_victims_num", "lga_victims_num"),
  url  = paste0(BASE_XLSX, c(
    "QLD_Reported_victims_Number.xlsx",
    "Region_Reported_victims_Number.xlsx",
    "District_Reported_victims_Number.xlsx",
    "LGA_Reported_victims_Number.xlsx"
  ))
)

for (i in seq_len(nrow(victims))) {
  nm   <- victims$name[i]
  path <- file.path("data/raw", paste0(nm, ".xlsx"))
  message("[", i, "/4] ", nm)

  raw <- as.data.table(
    readxl::read_excel(path, na = c("", "NA", "N/A", "-", "n.a."),
                       col_names = TRUE, .name_repair = "minimal")
  )
  non_empty_cols <- names(raw)[vapply(raw, function(x) any(!is.na(x)), logical(1L))]
  raw <- raw[rowSums(!is.na(raw)) > 0L, .SD, .SDcols = non_empty_cols]

  dt_proc <- tryCatch(process_dataset(raw, nm),
    error = function(e) { message("  ERROR: ", e$message); NULL })
  if (is.null(dt_proc)) next

  message("  Rows: ", nrow(dt_proc), " | Date range: ",
          min(dt_proc$date, na.rm = TRUE), " - ",
          max(dt_proc$date, na.rm = TRUE),
          " | FY: ", min(dt_proc$financial_year, na.rm = TRUE),
          " - ", max(dt_proc$financial_year, na.rm = TRUE))
  saveRDS(dt_proc, file.path("data/processed", paste0(nm, ".rds")))
  message("  Saved.")
}
