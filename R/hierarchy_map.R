# R/hierarchy_map.R
# Offence hierarchy for QPS crime statistics.
#   Level 1  main_group  : QGSO "Offence category 1"
#   Level 2  subgroup    : QGSO "Offence category 2"
#   Level 3  offence_col : column names as they appear in the QPS open data CSV/XLSX files
#
# is_subtotal = TRUE  -> this column is an aggregate; exclude when summing individuals
#
# PRIMARY SOURCE: offence_classification.csv in the project root (user-editable).
# Edit that file, then re-run R/01_download_and_process.R to apply changes.
# The pattern fallback below handles any column not listed in the CSV.

# ── Package installation ───────────────────────────────────────────────────────
required_pkgs <- c("data.table", "readxl", "lubridate")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(data.table)
library(lubridate)


# ── Pattern-based fallback (for columns not listed in offence_classification.csv) ──

classify_by_pattern <- function(col_name) {
  s <- tolower(col_name)

  if (grepl("murder|manslaughter|conspiracy to murder|driving causing death|unlawful striking", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Other Homicide",                      is_subtotal = FALSE))
  if (grepl("^homicide", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Homicide (Murder)",                   is_subtotal = FALSE))
  if (grepl("assault|grievous|bodily harm", s) && !grepl("sexual", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Assault",                             is_subtotal = FALSE))
  if (grepl("rape|sexual", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Sexual Offences",                     is_subtotal = FALSE))
  if (grepl("robbery", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Robbery",                             is_subtotal = FALSE))
  if (grepl("kidnap|abduction|extortion|stalking|harassment|coercive control|life endanger", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Other Offences Against the Person",   is_subtotal = FALSE))

  if (grepl("^stealing|other stealing|shop stealing|stealing from|other theft", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Other Theft (excl. Unlawful Entry)",  is_subtotal = FALSE))
  if (grepl("unlawful use of motor|motor vehicle theft", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Unlawful Use of Motor Vehicle",       is_subtotal = FALSE))
  if (grepl("unlawful entry|break.and.enter|break.&.enter|burglary", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Unlawful Entry",                      is_subtotal = FALSE))
  if (grepl("^arson", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Arson",                               is_subtotal = FALSE))
  if (grepl("wilful damage|property damage|graffiti", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Other Property Damage",               is_subtotal = FALSE))
  if (grepl("fraud|forgery|counterfeit", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Fraud",                               is_subtotal = FALSE))
  if (grepl("stolen goods|handling stolen|receiving stolen|tainted property|suspected stolen", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Handling Stolen Goods",               is_subtotal = FALSE))

  if (grepl("drug|traffick|narcotic|possess.*drug|produce.*drug|manufacture.*drug", s))
    return(list(main_group = "Other Offences", subgroup = "Drug Offences",                        is_subtotal = FALSE))
  if (grepl("weapon|firearm|bomb", s))
    return(list(main_group = "Other Offences", subgroup = "Weapons Act Offences",                 is_subtotal = FALSE))
  if (grepl("drink driving|dangerous operation|disqualified driving|interfere.*vehicle|^traffic", s))
    return(list(main_group = "Other Offences", subgroup = "Traffic and Related Offences",         is_subtotal = FALSE))
  if (grepl("domestic violence|dvpo|dvo|protection order", s))
    return(list(main_group = "Other Offences", subgroup = "Breach of DVO",                        is_subtotal = FALSE))
  if (grepl("liquor", s))
    return(list(main_group = "Other Offences", subgroup = "Liquor Offences (excl. Drunkenness)",  is_subtotal = FALSE))
  if (grepl("trespass|vagrancy|vagrant", s))
    return(list(main_group = "Other Offences", subgroup = "Trespassing and Vagrancy",             is_subtotal = FALSE))
  if (grepl("good order|disorderly|move.on|fare evasion|public nuisance|obstruct|resist", s))
    return(list(main_group = "Other Offences", subgroup = "Good Order Offences",                  is_subtotal = FALSE))
  if (grepl("prostitut|gaming|gambl|stock", s))
    return(list(main_group = "Other Offences", subgroup = "Miscellaneous Offences",               is_subtotal = FALSE))

  list(main_group = "Other Offences", subgroup = "Miscellaneous Offences", is_subtotal = FALSE)
}


# ── Build a full hierarchy data.table for a vector of column names ─────────────
# Loads offence_classification.csv from the project root as the primary lookup.
# Falls back to classify_by_pattern() for any column not listed in the CSV.
# Returns data.table with: offence_col, main_group, subgroup, is_subtotal

build_hierarchy <- function(col_names) {
  csv_path <- file.path(getwd(), "offence_classification.csv")
  if (!file.exists(csv_path))
    stop("offence_classification.csv not found in project root. ",
         "Run R/01_download_and_process.R from the project root directory.")

  lookup <- fread(csv_path, colClasses = list(character = c("offence", "main_group", "subgroup")))
  lookup[, is_subtotal := as.logical(is_subtotal)]
  setnames(lookup, "offence", "offence_col")
  setkey(lookup, offence_col)

  result <- data.table(offence_col = col_names)
  result <- lookup[result, on = "offence_col"]

  unmatched <- which(is.na(result$main_group))
  if (length(unmatched) > 0L) {
    fb <- lapply(result$offence_col[unmatched], classify_by_pattern)
    result[unmatched, main_group  := vapply(fb, `[[`, character(1L), "main_group")]
    result[unmatched, subgroup    := vapply(fb, `[[`, character(1L), "subgroup")]
    result[unmatched, is_subtotal := vapply(fb, `[[`, logical(1L),   "is_subtotal")]
  }

  result
}
