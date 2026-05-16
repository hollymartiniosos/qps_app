# QPS Crime Statistics — Open Data vs CS Data Comparison

A Shiny web application that places published Queensland Police Service (QPS) crime statistics side-by-side with synthetic CS data, so analysts can visually compare and validate the two datasets across offence types, geographic levels, and time periods.

---

## Quick start

```r
# 1. Download and process QPS open data (needs internet, ~2–5 min)
source("R/01_download_and_process.R")

# 2. Generate synthetic CS data
source("R/02_mock_data.R")

# 3. Launch the app
shiny::runApp("app", launch.browser = TRUE)
```

That's it — no manual data download required.

---

## Prerequisites

- **R ≥ 4.3** — [cran.r-project.org](https://cran.r-project.org/)
- **RStudio** (optional) — [posit.co](https://posit.co/download/rstudio-desktop/)
- Internet access for Step 1

All required R packages are installed automatically on first run.

---

## How the data is sourced

`R/01_download_and_process.R` downloads 18 datasets directly from the QPS open data S3 bucket:

```
https://open-crime-data.s3-ap-southeast-2.amazonaws.com/Crime Statistics/
```

The 18 files cover:

| Category | Geographic levels |
|----------|-------------------|
| Offences — count | QLD · Region · District · Division · LGA |
| Offences — rate per 100k | QLD · Region · District · Division · LGA |
| Offenders — count | QLD · Region · District · LGA |
| Victims — count | QLD · Region · District · LGA |

Files are saved to `data/raw/` on first download and reused on subsequent runs (no re-download if the file already exists).

The script then cleans and reshapes each file into a tidy long-format table and saves it to `data/processed/<name>.rds`.

`R/02_mock_data.R` reads each processed file and applies statistical perturbation to produce a synthetic CS dataset, saved to `data/mock/<name>_mock.rds`.

Both `data/` folders are git-ignored and stay local.

---

## Project structure

```
qps_app/
├── app/                        # Shiny application
│   ├── global.R                # Package loading, dataset catalogue, lazy loader
│   ├── ui.R                    # Top-level navigation
│   ├── server.R                # Module wiring
│   └── modules/
│       └── pair_module.R       # UI + server module (one instance per dataset pair)
├── R/                          # Data preparation scripts
│   ├── 01_download_and_process.R   # Download from QPS → data/processed/
│   ├── 02_mock_data.R              # Generate CS data → data/mock/
│   └── hierarchy_map.R             # Offence hierarchy lookup table
├── data/                       # Git-ignored — created by the R scripts
│   ├── raw/
│   ├── processed/
│   └── mock/
└── tests/                      # testthat test suite
```

---

## Using the app

### Navigation

The top navbar groups datasets into four menus — Offences (Count), Offences (Rate), Offenders, Victims — each with sub-tabs for QLD, Region, District, Division, and LGA where available.

Tabs load data on first visit only, so startup is fast.

### Sidebar filters

- **Offence Group** — only shows groups present in the selected dataset (e.g. Victims tabs show only "Offences Against the Person")
- **Subgroup** — narrows to specific crime subgroups
- **Financial Year** — defaults to the current Queensland FY (July–June); multi-select to compare years
- **Geographic / demographic filters** — appear automatically for datasets that include region, district, LGA, age, or sex breakdowns
- Selecting a specific item automatically removes "All" from the selector, and vice versa

### Charts

- **Left — Open QPS Data**: published crime statistics
- **Right — CS Data**: synthetic counterpart
- Hover a bar to see category, month, and count
- Click a legend item to show/hide that series
- Select exactly **one subgroup** to drill down to individual offence level (e.g. "Common Assault", "Serious Assault")

### Comparison tables

Three tabs below the charts:

| Tab | Grouped by |
|-----|-----------|
| By Subgroup | Subgroup (with TOTAL row) |
| By Offence | Individual offence + subgroup |
| By Month | Calendar month + financial year |

% Difference is colour-coded: green = CS higher, red = CS lower.

---

## Running the tests

```r
source("tests/run_tests.R")
# Results written to tests/test_log.txt
```
