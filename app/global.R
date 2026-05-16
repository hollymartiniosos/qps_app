# app/global.R
# Loaded automatically by Shiny before ui.R and server.R.
# Handles package installation, data loading, and shared constants.

# ── Package installation ───────────────────────────────────────────────────────
pkgs <- c("shiny", "data.table", "ggplot2", "DT", "scales", "bslib", "plotly")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

library(shiny)
library(data.table)
library(ggplot2)
library(DT)
library(scales)
library(bslib)
library(plotly)

source("modules/pair_module.R")


# ── Dataset catalogue ──────────────────────────────────────────────────────────

DATASET_INFO <- data.table(
  name   = c(
    "qld_offences_num",      "region_offences_num",    "district_offences_num",
    "division_offences_num", "lga_offences_num",
    "qld_offences_rate",     "region_offences_rate",   "district_offences_rate",
    "division_offences_rate","lga_offences_rate",
    "qld_offenders_num",     "region_offenders_num",   "district_offenders_num",
    "lga_offenders_num",
    "qld_victims_num",       "region_victims_num",     "district_victims_num",
    "lga_victims_num"
  ),
  label  = c(
    "QLD", "Region", "District", "Division", "LGA",
    "QLD", "Region", "District", "Division", "LGA",
    "QLD", "Region", "District", "LGA",
    "QLD", "Region", "District", "LGA"
  ),
  group  = c(
    rep("Offences (Count)", 5L), rep("Offences (Rate)", 5L),
    rep("Offenders",        4L), rep("Victims",         4L)
  ),
  metric = c(rep("Number", 5L), rep("Rate", 5L), rep("Number", 8L))
)


# ── Lazy data loader (reads from disk on first access, caches in memory) ──────

DATA_CACHE <- new.env(parent = emptyenv())

DATA_FILES <- setNames(
  lapply(DATASET_INFO$name, function(nm) {
    qps_path <- file.path("..", "data", "qps", paste0(nm, ".rds"))
    cs_path  <- file.path("..", "data", "cs",  paste0(nm, ".rds"))
    if (file.exists(qps_path) && file.exists(cs_path)) list(qps = qps_path, cs = cs_path)
    else NULL
  }),
  DATASET_INFO$name
)
DATA_FILES   <- Filter(Negate(is.null), DATA_FILES)
DATASET_INFO <- DATASET_INFO[name %in% names(DATA_FILES)]

load_pair <- function(nm) {
  if (!exists(nm, envir = DATA_CACHE, inherits = FALSE)) {
    paths <- DATA_FILES[[nm]]
    qps   <- readRDS(paths$qps); if (!is.data.table(qps)) setDT(qps)
    cs    <- readRDS(paths$cs);  if (!is.data.table(cs))  setDT(cs)
    assign(nm, list(qps = qps, cs = cs), envir = DATA_CACHE)
  }
  get(nm, envir = DATA_CACHE, inherits = FALSE)
}


# ── Fixed colour palette (one colour per subgroup) ────────────────────────────

# Queensland Government brand palette
# Primary: Maroon #78003F  Supporting: Blue #005DA6  Gold #F5A623
SUBGROUP_COLORS <- c(
  "Homicide"                = "#78003F",  # QLD Maroon
  "Assault"                 = "#C4007A",  # Deep Magenta
  "Sexual Offences"         = "#8B1A4A",  # Dark Berry
  "Robbery"                 = "#E5590F",  # QLD Orange
  "Other Against Person"    = "#FF8C00",  # Amber
  "Unlawful Entry"          = "#005DA6",  # QLD Blue
  "Arson & Property Damage" = "#CB2B3B",  # Red
  "Fraud & Related"         = "#1565C0",  # Dark Blue
  "Stealing & Theft"        = "#0288D1",  # Sky Blue
  "Drug Offences"           = "#007478",  # QLD Teal
  "Weapons Offences"        = "#489A4A",  # QLD Green
  "Traffic Offences"        = "#F5A623",  # QLD Gold
  "Domestic Violence"       = "#AD1457",  # Crimson Pink
  "Good Order & Trespass"   = "#546E7A",  # Blue Grey
  "Liquor Offences"         = "#6D4C41",  # Dark Brown
  "Miscellaneous Offences"  = "#9E9E9E"   # Mid Grey
)
