# QPS Crime Statistics — Real vs Synthetic Data Comparison

A Shiny web application that places Open QPS (Queensland Police Service) crime statistics side-by-side with synthetic CS data, allowing analysts to visually compare and validate the two datasets across offence types, geographic levels, and time periods.

---

## Contents

```
qps_app/
├── app/                        # Shiny application
│   ├── global.R                # Package loading, data catalogue, lazy loader
│   ├── ui.R                    # Navigation structure
│   ├── server.R                # Module wiring
│   └── modules/
│       └── pair_module.R       # Reusable UI + server module (one per dataset pair)
├── R/                          # Data preparation scripts (run once)
│   ├── 01_download_and_process.R   # Parse raw CSVs/Excel → processed RDS
│   ├── 02_mock_data.R              # Generate synthetic CS data → mock RDS
│   └── hierarchy_map.R             # Offence hierarchy lookup table
├── data/                       # Created by the R scripts (git-ignored)
│   ├── raw/                    # Source files downloaded from QPS open data
│   ├── processed/              # Cleaned RDS files (one per dataset)
│   └── mock/                   # Synthetic CS RDS files (one per dataset)
└── tests/                      # testthat test suite
```

---

## Prerequisites

- **R ≥ 4.3** — [Download R](https://cran.r-project.org/)
- **RStudio** (optional but recommended) — [Download RStudio](https://posit.co/download/rstudio-desktop/)
- Internet access for first run (packages install automatically; raw data downloaded by script)

No manual package installation is needed — `global.R` installs missing packages on first launch.

---

## Step 1 — Prepare the raw data

Place the raw source files under `data/raw/`. The expected structure mirrors the QPS open data portal exports:

```
data/raw/
├── offences/
│   ├── qld_offences.csv            # State-wide offence counts
│   ├── region_offences.csv         # By Police Region
│   ├── district_offences.csv       # By Police District
│   ├── division_offences.csv       # By Police Division
│   ├── lga_offences.csv            # By Local Government Area
│   ├── qld_offences_rate.csv       # (same geography set, rate per 100k)
│   ├── ...
├── offenders/
│   ├── qld_offenders.csv
│   ├── region_offenders.csv
│   ├── district_offenders.csv
│   └── lga_offenders.csv
└── victims/
    ├── qld_victims.xlsx            # Victims files are Excel format
    ├── region_victims.xlsx
    ├── district_victims.xlsx
    └── lga_victims.xlsx
```

### Required columns in every source file

| Column | Description |
|--------|-------------|
| A date/month column | Month of recording — any of `%b %Y`, `%m/%Y`, `%Y-%m-%d` formats accepted |
| Offence hierarchy columns | The file must contain `main_group`, `subgroup`, and individual offence columns as supplied by QPS open data |
| Geographic column(s) | e.g. `region_name`, `district_name`, `lga_name` — present only in the relevant geographic files |

The scripts are tolerant of minor formatting variation (quoted headers, extra columns, BOM encoding). Do not rename or restructure the QPS portal export — use the files as downloaded.

---

## Step 2 — Process the raw data

Open an R console in the project root and run:

```r
source("R/01_download_and_process.R")
```

This reads every file in `data/raw/`, standardises the schema, and writes one `.rds` file per dataset into `data/processed/`.  
Expected runtime: 2–5 minutes depending on machine speed.

---

## Step 3 — Generate synthetic CS data

```r
source("R/02_mock_data.R")
```

This reads each processed `.rds`, applies statistical perturbation to simulate a CS dataset, and writes the results to `data/mock/`.  
Each mock file is named `<dataset>_mock.rds`.

---

## Step 4 — Run the app

From the project root:

```r
shiny::runApp("app", launch.browser = TRUE)
```

Or open `app/global.R` in RStudio and click **Run App**.

---

## How the app works

### Navigation

The top navbar groups datasets into four menus:

| Menu | Datasets |
|------|----------|
| Offences (Count) | QLD · Region · District · Division · LGA |
| Offences (Rate) | QLD · Region · District · Division · LGA |
| Offenders | QLD · Region · District · LGA |
| Victims | QLD · Region · District · LGA |

Each tab is **independently loaded** — data for a tab is only read from disk the first time you visit it, keeping startup fast.

### Sidebar filters

- **Offence Group** — only shows groups present in that dataset (e.g. Victims tabs show only "Offences Against the Person")
- **Subgroup** — filtered by the selected offence groups
- **Financial Year** — defaults to the current Queensland financial year (July–June)
- **Geographic / demographic filters** — appear automatically when the dataset contains region, district, LGA, or demographic breakdowns
- Selecting any specific item automatically removes "All" from the selector (and vice versa)

### Charts

- Left: **Open QPS Data** — the published crime statistics
- Right: **CS Data** — the synthetic counterpart
- Hovering a bar shows category, month, and count
- Clicking a legend item shows/hides that series
- Selecting exactly **one subgroup** drills the chart down to individual offence level (e.g. "Common Assault", "Serious Assault")

### Comparison tables

Three tabs below the charts aggregate the filtered data:

| Tab | Rows grouped by |
|-----|----------------|
| By Subgroup | Subgroup (with TOTAL row) |
| By Offence | Individual offence + subgroup |
| By Month | Calendar month + financial year |

Columns: Real count · Mock count · Difference · % Difference (green = mock higher, red = mock lower).

---

## Running the test suite

```r
source("tests/run_tests.R")
```

Results are written to `tests/test_log.txt`. The suite covers:
1. Offence hierarchy mapping
2. Processed and mock file schema validation
3. Module filter behaviour
4. Comparison table calculations
