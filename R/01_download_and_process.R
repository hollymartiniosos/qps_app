# R/01_download_and_process.R
# Downloads all 18 QPS crime-statistics datasets and transforms each into a
# tidy (long) data.table with offence-hierarchy columns attached.
#
# Output:  data/processed/<name>.rds   (one data.table per dataset)
# Run from project root:  source("R/01_download_and_process.R")

source("R/hierarchy_map.R")   # also handles package installation
library(readxl)


# ── Dataset catalogue ──────────────────────────────────────────────────────────

BASE_CSV  <- "https://open-crime-data.s3-ap-southeast-2.amazonaws.com/Crime%20Statistics/"
BASE_XLSX <- "https://open-crime-data.s3.ap-southeast-2.amazonaws.com/Crime%20Statistics/"

datasets <- data.table(
  name = c(
    # Offences – number
    "qld_offences_num",      "region_offences_num",    "district_offences_num",
    "division_offences_num", "lga_offences_num",
    # Offences – rate
    "qld_offences_rate",     "region_offences_rate",   "district_offences_rate",
    "division_offences_rate","lga_offences_rate",
    # Offenders – number
    "qld_offenders_num",     "region_offenders_num",   "district_offenders_num",
    "lga_offenders_num",
    # Victims – number (Excel)
    "qld_victims_num",       "region_victims_num",     "district_victims_num",
    "lga_victims_num"
  ),
  url = c(
    paste0(BASE_CSV,  "QLD_Reported_Offences_Number.csv"),
    paste0(BASE_CSV,  "region_Reported_Offences_Number.csv"),
    paste0(BASE_CSV,  "district_Reported_Offences_Number.csv"),
    paste0(BASE_CSV,  "division_Reported_Offences_Number.csv"),
    paste0(BASE_CSV,  "LGA_Reported_Offences_Number.csv"),
    paste0(BASE_CSV,  "QLD_Reported_Offences_Rates.csv"),
    paste0(BASE_CSV,  "region_Reported_Offences_Rates.csv"),
    paste0(BASE_CSV,  "district_Reported_Offences_Rates.csv"),
    paste0(BASE_CSV,  "division_Reported_Offences_Rates.csv"),
    paste0(BASE_CSV,  "LGA_Reported_Offences_Rates.csv"),
    paste0(BASE_CSV,  "QLD_Reported_Offenders_Number.csv"),
    paste0(BASE_CSV,  "Region_Reported_Offenders_Number.csv"),
    paste0(BASE_CSV,  "district_Reported_Offenders_Number.csv"),
    paste0(BASE_CSV,  "LGA_Reported_Offenders_Number.csv"),
    paste0(BASE_XLSX, "QLD_Reported_victims_Number.xlsx"),
    paste0(BASE_XLSX, "Region_Reported_victims_Number.xlsx"),
    paste0(BASE_XLSX, "District_Reported_victims_Number.xlsx"),
    paste0(BASE_XLSX, "LGA_Reported_victims_Number.xlsx")
  ),
  ext = c(rep("csv", 14L), rep("xlsx", 4L))
)


# ── Directory setup ────────────────────────────────────────────────────────────

dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)


# ── Known non-offence column names ────────────────────────────────────────────

GEO_COLS  <- c("Region", "District", "LGA Name", "LGA", "Division",
               "Police Region", "Police District", "Police Division")
DEMO_COLS <- c("Age", "Sex", "Gender",
               "Offender Age", "Offender Sex",
               "Victim Age",   "Victim Sex")
DATE_COLS <- c("Month Year", "MonthYear", "Month_Year", "Date", "Period")


# ── Helper: download one file ──────────────────────────────────────────────────

download_raw <- function(url, name, ext) {
  dest <- file.path("data/raw", paste0(name, ".", ext))
  if (!file.exists(dest)) {
    message("  Downloading ", name, " …")
    tryCatch(
      download.file(url, destfile = dest, mode = "wb", quiet = TRUE),
      error = function(e) {
        message("  FAILED: ", e$message)
        if (file.exists(dest)) file.remove(dest)
      }
    )
  }
  dest
}


# ── Helper: read CSV or XLSX into a data.table ────────────────────────────────

read_raw <- function(path, ext) {
  if (ext == "xlsx") {
    dt <- as.data.table(
      readxl::read_excel(path,
                         na          = c("", "NA", "N/A", "-", "n.a."),
                         col_names   = TRUE,
                         .name_repair = "minimal")
    )
    # Drop rows/columns that are entirely NA (artefacts of merged Excel cells)
    non_empty_cols <- names(dt)[vapply(dt, function(x) any(!is.na(x)), logical(1L))]
    dt <- dt[rowSums(!is.na(dt)) > 0L, .SD, .SDcols = non_empty_cols]
  } else {
    dt <- fread(path,
                na.strings   = c("", "NA", "N/A", "-", "n.a."),
                check.names  = FALSE,
                header       = TRUE,
                showProgress = FALSE,
                encoding     = "UTF-8")
    # fread sometimes skips the true header and treats the first data row as
    # column names.  Detect this: if col 1 is NOT a known id-column name, the
    # first line of the file is the real header but fread missed it.
    all_id_names <- c(GEO_COLS, DEMO_COLS, DATE_COLS)
    if (!names(dt)[1L] %in% all_id_names) {
      # Read the true header (row 1) via a proper CSV parse (fill handles
      # any trailing commas; header=FALSE treats everything as data).
      hdr_raw  <- fread(path, header = FALSE, nrows = 1L,
                        showProgress = FALSE, encoding = "UTF-8",
                        fill = TRUE)
      true_names <- as.character(hdr_raw[1L])
      # Read full data (skip the header row we'll supply manually)
      dt <- fread(path,
                  na.strings   = c("", "NA", "N/A", "-", "n.a."),
                  check.names  = FALSE,
                  header       = FALSE,
                  skip         = 1L,
                  showProgress = FALSE,
                  encoding     = "UTF-8",
                  fill         = TRUE)
      # If the data has more columns than the header (unlabelled trailing
      # columns), pad with synthetic names rather than error.
      n_data <- ncol(dt)
      n_hdr  <- length(true_names)
      if (n_data > n_hdr)
        true_names <- c(true_names,
                        paste0("V_extra_", seq_len(n_data - n_hdr)))
      setnames(dt, true_names[seq_len(n_data)])
    }
  }
  dt
}


# ── Helper: identify date / geography / demographic columns ───────────────────

detect_id_cols <- function(dt) {
  cols     <- names(dt)
  date_col <- intersect(cols, DATE_COLS)[1L]
  if (is.na(date_col)) date_col <- cols[1L]   # fallback: first column
  geo_cols  <- intersect(cols, GEO_COLS)
  demo_cols <- intersect(cols, DEMO_COLS)
  list(
    date_col = date_col,
    geo_cols  = geo_cols,
    demo_cols = demo_cols,
    all_id    = unique(c(date_col, geo_cols, demo_cols))
  )
}


# ── Helper: parse "JAN97" / "JAN 1997" / "01/1997" to Date ───────────────────

parse_month_year <- function(x) {
  x_str  <- trimws(as.character(x))
  parsed <- lubridate::parse_date_time(
    x_str,
    orders = c("Ymd", "ymd", "my", "mY", "b y", "b Y", "b-y", "b-Y", "Y-m", "m/Y"),
    locale = "en_US",
    quiet  = TRUE
  )
  # Last-resort: prepend "01 " and parse as day-month-year
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


# ── Core processing function ───────────────────────────────────────────────────

process_dataset <- function(dt, name) {
  if (!is.data.table(dt)) setDT(dt)

  id_info <- detect_id_cols(dt)

  # Candidate offence columns = everything that isn't an id column
  offence_cols <- setdiff(names(dt), id_info$all_id)

  # Keep only columns whose values are predominantly numeric
  is_numeric_col <- function(col) {
    vals <- dt[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0L) return(FALSE)
    suppressWarnings(mean(!is.na(as.numeric(as.character(vals)))) > 0.5)
  }
  offence_cols <- offence_cols[vapply(offence_cols, is_numeric_col, logical(1L))]

  if (length(offence_cols) == 0L)
    stop("No numeric offence columns found after filtering.")

  # Build offence hierarchy for this dataset's columns
  hier <- build_hierarchy(offence_cols)

  # Subset to id + offence columns, then melt to long format
  dt_sub  <- dt[, .SD, .SDcols = c(id_info$all_id, offence_cols)]
  dt_long <- melt(dt_sub,
                  id.vars         = id_info$all_id,
                  measure.vars    = offence_cols,
                  variable.name   = "offence",
                  value.name      = "count",
                  variable.factor = FALSE)

  # Coerce count to numeric (handles any stray commas / text)
  dt_long[, count := suppressWarnings(as.numeric(gsub(",", "", as.character(count))))]

  # Attach hierarchy columns via keyed join
  setkey(hier, offence_col)
  dt_long[hier, on = c(offence = "offence_col"),
          `:=`(main_group  = i.main_group,
               subgroup    = i.subgroup,
               is_subtotal = i.is_subtotal)]

  # Parse dates and derive time fields
  dt_long[, date           := parse_month_year(get(id_info$date_col))]
  dt_long[, month          := month(date)]
  dt_long[, year           := year(date)]
  dt_long[, financial_year := fifelse(
    month >= 7L,
    paste0(year,      "-", year + 1L),
    paste0(year - 1L, "-", year)
  )]

  # Warn about any columns that ended up unclassified
  unknown <- unique(dt_long[is.na(main_group), offence])
  if (length(unknown) > 0L)
    warning(name, ": ", length(unknown), " unclassified column(s): ",
            paste(head(unknown, 6L), collapse = ", "),
            if (length(unknown) > 6L) " …" else "")

  # Attach dataset-level metadata (useful in the Shiny app)
  dt_long[, dataset_name := name]
  dt_long[, geo_cols     := paste(id_info$geo_cols,  collapse = "|")]
  dt_long[, demo_cols    := paste(id_info$demo_cols, collapse = "|")]

  dt_long
}


# ── Main pipeline ──────────────────────────────────────────────────────────────

processed <- vector("list", nrow(datasets))
names(processed) <- datasets$name

for (i in seq_len(nrow(datasets))) {
  d <- datasets[i]
  message("\n[", i, "/", nrow(datasets), "] ", d$name)

  dest <- download_raw(d$url, d$name, d$ext)
  if (!file.exists(dest)) { message("  Skipped (download failed)"); next }

  dt_raw <- tryCatch(
    read_raw(dest, d$ext),
    error = function(e) { message("  Read ERROR: ", e$message); NULL }
  )
  if (is.null(dt_raw)) next
  message("  Raw: ", nrow(dt_raw), " rows × ", ncol(dt_raw), " cols")

  dt_proc <- tryCatch(
    process_dataset(dt_raw, d$name),
    error = function(e) { message("  Process ERROR: ", e$message); NULL }
  )
  if (is.null(dt_proc)) next

  message("  Long: ", nrow(dt_proc), " rows | ",
          uniqueN(dt_proc$offence), " offences | ",
          "Date range: ", min(dt_proc$date, na.rm = TRUE),
          " – ", max(dt_proc$date, na.rm = TRUE))
  message("  Geo cols : ", unique(dt_proc$geo_cols))
  message("  Demo cols: ", unique(dt_proc$demo_cols))

  out_path <- file.path("data/processed", paste0(d$name, ".rds"))
  saveRDS(dt_proc, out_path)
  message("  Saved -> ", out_path)

  processed[[d$name]] <- dt_proc
}


# ── Summary ────────────────────────────────────────────────────────────────────

ok <- !vapply(processed, is.null, logical(1L))
cat("\n===== PROCESSING COMPLETE =====\n")
cat("Processed:", sum(ok), "/", nrow(datasets), "datasets\n\n")

for (nm in names(processed)[ok]) {
  dt <- processed[[nm]]
  cat(sprintf("%-30s  rows: %8d  offences: %3d  fy: %s – %s\n",
              nm, nrow(dt), uniqueN(dt$offence),
              min(dt$financial_year, na.rm = TRUE),
              max(dt$financial_year, na.rm = TRUE)))
}

# Print hierarchy review for the first successful dataset
first_ok <- names(processed)[ok][1L]
if (!is.null(first_ok)) {
  cat("\nOffence -> hierarchy mapping (", first_ok, "):\n", sep = "")
  review <- unique(processed[[first_ok]][, .(offence, main_group, subgroup, is_subtotal)])
  setorder(review, main_group, subgroup, offence)
  print(review, nrows = Inf)
}

cat("\nNext step: source('R/02_mock_data.R')\n")

