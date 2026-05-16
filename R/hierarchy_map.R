# R/hierarchy_map.R
# Offence hierarchy for QPS crime statistics.
#   Level 1  main_group  : Offences Against the Person | Offences Against Property | Other Offences
#   Level 2  subgroup    : Homicide | Assault | Robbery | Unlawful Entry | Drug Offences | …
#   Level 3  offence_col : individual column names as they appear in the CSV/XLSX files
#
# is_subtotal = TRUE  ->  this column aggregates sub-types (exclude when summing individuals)

# ── Package installation ───────────────────────────────────────────────────────
required_pkgs <- c("data.table", "readxl", "lubridate")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(data.table)
library(lubridate)

# ── Exact column-name lookup table ─────────────────────────────────────────────

offence_hierarchy_exact <- data.table(
  offence_col = c(
    # Homicide (9)
    "Homicide (Murder)", "Murder", "Other Homicide", "Attempted Murder",
    "Conspiracy to Murder", "Manslaughter (excl. by driving)",
    "Manslaughter Unlawful Striking Causing Death", "Driving Causing Death",
    "Homicide",
    # Assault (7)
    "Grievous Assault", "Serious Assault", "Serious Assault (Not Sexually Motivated)",
    "Serious Assault (Other)", "Sexually Motivated Assault", "Common Assault",
    "Assault",
    # Sexual Offences (3)
    "Rape and Attempted Rape", "Other Sexual Offences", "Sexual Offences",
    # Robbery (3)
    "Armed Robbery", "Unarmed Robbery", "Robbery",
    # Other Against Person (6)
    "Kidnapping/Abduction", "Extortion", "Stalking", "Harassment",
    "Other Offences Against the Person", "Other Offences Against Person",
    # Unlawful Entry (5)
    "Unlawful Entry With Violence - Dwelling", "Unlawful Entry Without Violence - Dwelling",
    "Unlawful Entry With Violence - Other",    "Unlawful Entry Without Violence - Other",
    "Unlawful Entry",
    # Arson & Property Damage (4)
    "Arson", "Wilful Damage", "Other Property Damage", "Property Damage",
    # Fraud & Related (5)
    "Fraud", "Forgery/Counterfeiting", "Forgery", "Receiving/Handling Stolen Goods",
    "Fraud Offences",
    # Stealing & Theft (10)
    "Stealing from Dwelling", "Stealing from Vehicle (excl. parts)", "Stealing from Vehicle",
    "Stealing from Shop", "Stealing (Motor Vehicle Parts)", "Other Stealing",
    "Stealing", "Unlawful Use of Motor Vehicle", "Motor Vehicle Theft",
    "Theft & Related Offences",
    # Drug Offences (5)
    "Trafficking in Dangerous Drugs", "Possession and/or Use of Dangerous Drugs",
    "Produce/Manufacture/Supply of Dangerous Drugs", "Other Drug Offences",
    "Drug Offences",
    # Weapons (2)
    "Weapons Act Offences", "Weapons",
    # Traffic (5)
    "Drink Driving", "Dangerous Operation of a Vehicle", "Disqualified Driving",
    "Interfere with Mechanism of Motor Vehicle", "Traffic Offences",
    # Domestic Violence (1)
    "Breach of Domestic Violence Order",
    # Good Order & Trespass (2)
    "Good Order Offences", "Trespass",
    # Prostitution (2)
    "Prostitution Offences", "Prostitution",
    # Liquor & Gaming (2)
    "Liquor Act Offences", "Gaming",
    # Catch-all other (3)
    "Stock Related Offences", "Miscellaneous Offences", "Other Offences"
  ),
  main_group = c(
    rep("Offences Against the Person",  9L),   # Homicide
    rep("Offences Against the Person",  7L),   # Assault
    rep("Offences Against the Person",  3L),   # Sexual Offences
    rep("Offences Against the Person",  3L),   # Robbery
    rep("Offences Against the Person",  6L),   # Other Against Person
    rep("Offences Against Property",    5L),   # Unlawful Entry
    rep("Offences Against Property",    4L),   # Arson & Property Damage
    rep("Offences Against Property",    5L),   # Fraud & Related
    rep("Offences Against Property",   10L),   # Stealing & Theft
    rep("Other Offences",               5L),   # Drug Offences
    rep("Other Offences",               2L),   # Weapons
    rep("Other Offences",               5L),   # Traffic
    rep("Other Offences",               1L),   # Domestic Violence
    rep("Other Offences",               2L),   # Good Order & Trespass
    rep("Other Offences",               2L),   # Prostitution
    rep("Other Offences",               2L),   # Liquor & Gaming
    rep("Other Offences",               3L)    # Catch-all
  ),
  subgroup = c(
    rep("Homicide",              9L),
    rep("Assault",               7L),
    rep("Sexual Offences",       3L),
    rep("Robbery",               3L),
    rep("Other Against Person",  6L),
    rep("Unlawful Entry",        5L),
    rep("Arson & Property Damage", 4L),
    rep("Fraud & Related",       5L),
    rep("Stealing & Theft",     10L),
    rep("Drug Offences",         5L),
    rep("Weapons Offences",      2L),
    rep("Traffic Offences",      5L),
    rep("Domestic Violence",     1L),
    rep("Good Order & Trespass", 2L),
    rep("Prostitution",          2L),
    rep("Liquor & Gaming",       2L),
    rep("Other Offences",        3L)
  ),
  is_subtotal = c(
    # Homicide:   last is subtotal
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
    # Assault:    last is subtotal
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
    # Sexual:     last is subtotal
    FALSE, FALSE, TRUE,
    # Robbery:    last is subtotal
    FALSE, FALSE, TRUE,
    # Other Person: none
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    # Unlawful Entry: last is subtotal
    FALSE, FALSE, FALSE, FALSE, TRUE,
    # Arson:      last is subtotal
    FALSE, FALSE, FALSE, TRUE,
    # Fraud:      last is subtotal
    FALSE, FALSE, FALSE, FALSE, TRUE,
    # Stealing:   last is subtotal
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
    # Drug:       last is subtotal
    FALSE, FALSE, FALSE, FALSE, TRUE,
    # Weapons:    none
    FALSE, FALSE,
    # Traffic:    last is subtotal
    FALSE, FALSE, FALSE, FALSE, TRUE,
    # DV:         none
    FALSE,
    # Good Order: none
    FALSE, FALSE,
    # Prostitution: none
    FALSE, FALSE,
    # Liquor:     none
    FALSE, FALSE,
    # Catch-all:  none
    FALSE, FALSE, FALSE
  )
)

setkey(offence_hierarchy_exact, offence_col)


# ── Pattern-based fallback (for columns not in the exact table) ────────────────

classify_by_pattern <- function(col_name) {
  s <- tolower(col_name)

  if (grepl("murder|manslaughter|homicide|conspiracy to murder|driving causing death", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Homicide",             is_subtotal = FALSE))
  if (grepl("assault|grievous|bodily harm", s) && !grepl("sexual", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Assault",              is_subtotal = FALSE))
  if (grepl("rape|sexual", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Sexual Offences",      is_subtotal = FALSE))
  if (grepl("robbery", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Robbery",              is_subtotal = FALSE))
  if (grepl("kidnap|abduction|extortion|stalking|harassment", s))
    return(list(main_group = "Offences Against the Person", subgroup = "Other Against Person", is_subtotal = FALSE))

  if (grepl("unlawful entry|break.and.enter|break.&.enter|burglary", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Unlawful Entry",       is_subtotal = FALSE))
  if (grepl("arson|wilful damage|property damage", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Arson & Property Damage", is_subtotal = FALSE))
  if (grepl("fraud|forgery|counterfeit|stolen goods|handling stolen|receiving stolen", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Fraud & Related",      is_subtotal = FALSE))
  if (grepl("stealing|theft|unlawful use of motor|motor vehicle theft", s))
    return(list(main_group = "Offences Against Property",   subgroup = "Stealing & Theft",     is_subtotal = FALSE))

  if (grepl("drug|traffick|narcotic|possession.*drug|produce.*drug|manufacture.*drug", s))
    return(list(main_group = "Other Offences",              subgroup = "Drug Offences",        is_subtotal = FALSE))
  if (grepl("weapon", s))
    return(list(main_group = "Other Offences",              subgroup = "Weapons Offences",     is_subtotal = FALSE))
  if (grepl("drink driving|dangerous operation|disqualified driving|interfere.*vehicle", s))
    return(list(main_group = "Other Offences",              subgroup = "Traffic Offences",     is_subtotal = FALSE))
  if (grepl("domestic violence|dvpo|dvo|protection order", s))
    return(list(main_group = "Other Offences",              subgroup = "Domestic Violence",    is_subtotal = FALSE))
  if (grepl("prostitut", s))
    return(list(main_group = "Other Offences",              subgroup = "Prostitution",         is_subtotal = FALSE))
  if (grepl("liquor|gaming|gambl", s))
    return(list(main_group = "Other Offences",              subgroup = "Liquor & Gaming",      is_subtotal = FALSE))
  if (grepl("trespass|good order|disorderly", s))
    return(list(main_group = "Other Offences",              subgroup = "Good Order & Trespass", is_subtotal = FALSE))
  if (grepl("stock", s))
    return(list(main_group = "Other Offences",              subgroup = "Other Offences",       is_subtotal = FALSE))

  list(main_group = "Other Offences", subgroup = "Other Offences", is_subtotal = FALSE)
}


# ── Build a full hierarchy data.table for a vector of column names ─────────────
# Returns data.table with: offence_col, main_group, subgroup, is_subtotal

build_hierarchy <- function(col_names) {
  result <- data.table(offence_col = col_names)
  result <- offence_hierarchy_exact[result, on = "offence_col"]   # left join

  unmatched <- which(is.na(result$main_group))
  if (length(unmatched) > 0L) {
    fb <- lapply(result$offence_col[unmatched], classify_by_pattern)
    result[unmatched, main_group  := vapply(fb, `[[`, character(1L), "main_group")]
    result[unmatched, subgroup    := vapply(fb, `[[`, character(1L), "subgroup")]
    result[unmatched, is_subtotal := vapply(fb, `[[`, logical(1L),   "is_subtotal")]
  }

  result
}
