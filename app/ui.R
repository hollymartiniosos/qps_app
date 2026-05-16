library(shiny)
library(bslib)

# Suppress lintr false-positives for data.table NSE column references
# and Shiny globals loaded via global.R / modules/pair_module.R
utils::globalVariables(c("DATASET_INFO", "group", "name", "label",
                          "pair_ui", "DATA_FILES"))

# Build one navbarMenu per group; skip groups with no available data
make_menu <- function(group_name) {
  rows <- DATASET_INFO[group == group_name]
  if (nrow(rows) == 0L) return(NULL)
  tabs <- lapply(seq_len(nrow(rows)), function(i) {
    tabPanel(rows$label[i], pair_ui(rows$name[i]))
  })
  do.call(navbarMenu, c(list(group_name), tabs))
}

menu_list <- Filter(Negate(is.null), list(
  make_menu("Offences (Count)"),
  make_menu("Offences (Rate)"),
  make_menu("Offenders"),
  make_menu("Victims")
))

do.call(
  navbarPage,
  c(
    list(
      title  = "QPS Crime Statistics",
      theme  = bs_theme(bootswatch = "flatly"),
      header = tags$head(
        tags$link(rel = "shortcut icon", href = "favicon.ico"),
        tags$link(
          rel  = "stylesheet",
          href = "https://fonts.googleapis.com/css2?family=Noto+Sans:wght@400;600;700&display=swap"
        ),
        tags$style(HTML("
          /* ── Global font ──────────────────────────────────────────── */
          body, .navbar, .nav, input, select, button, label, .dataTables_wrapper {
            font-family: 'Noto Sans', sans-serif !important;
          }

          /* ── Navbar background & text ─────────────────────────────── */
          .navbar, .navbar-default {
            background-color: #005eb8 !important;
            border-color:     #004f9e !important;
          }
          .navbar-default .navbar-brand,
          .navbar-default .navbar-brand:hover,
          .navbar-default .navbar-brand:focus {
            color: #ffffff !important;
            font-weight: 700;
            font-size: 1.05rem;
            letter-spacing: 0.02em;
          }
          .navbar-default .navbar-nav > li > a {
            color: #ffffff !important;
          }
          .navbar-default .navbar-nav > li > a:hover,
          .navbar-default .navbar-nav > li > a:focus {
            color: #ffffff !important;
            background-color: #004f9e !important;
          }
          .navbar-default .navbar-nav > .active > a,
          .navbar-default .navbar-nav > .active > a:hover,
          .navbar-default .navbar-nav > .active > a:focus,
          .navbar-default .navbar-nav > .open  > a,
          .navbar-default .navbar-nav > .open  > a:hover,
          .navbar-default .navbar-nav > .open  > a:focus {
            color:            #ffffff !important;
            background-color: #003d82 !important;
          }

          /* ── Dropdown menus ───────────────────────────────────────── */
          .navbar-default .dropdown-menu {
            background-color: #005eb8;
            border-color:     #004f9e;
          }
          .navbar-default .dropdown-menu > li > a {
            color: #ffffff !important;
          }
          .navbar-default .dropdown-menu > li > a:hover,
          .navbar-default .dropdown-menu > li > a:focus {
            color:            #ffffff !important;
            background-color: #004f9e !important;
          }
          .navbar-default .dropdown-menu > .active > a,
          .navbar-default .dropdown-menu > .active > a:hover {
            background-color: #003d82 !important;
            color:            #ffffff !important;
          }

          /* ── Comparison tabset tabs ───────────────────────────────── */
          .nav-tabs > li > a {
            color: #005eb8 !important;
          }
          .nav-tabs > li.active > a,
          .nav-tabs > li.active > a:hover,
          .nav-tabs > li.active > a:focus {
            color: #005eb8 !important;
            border-top: 2px solid #005eb8 !important;
          }
          .nav-tabs > li > a:hover {
            color: #003d82 !important;
          }

          /* ── DataTables pagination buttons ───────────────────────── */
          .dataTables_paginate .paginate_button > a,
          .dataTables_paginate .paginate_button > a:visited {
            background-color: #005eb8 !important;
            border-color:     #005eb8 !important;
            color:            #ffffff !important;
          }
          .dataTables_paginate .paginate_button.current > a,
          .dataTables_paginate .paginate_button.current > a:hover {
            background-color: #003d82 !important;
            border-color:     #003d82 !important;
            color:            #ffffff !important;
          }
          .dataTables_paginate .paginate_button:not(.disabled) > a:hover,
          .dataTables_paginate .paginate_button:not(.disabled) > a:focus {
            background-color: #004f9e !important;
            border-color:     #004f9e !important;
            color:            #ffffff !important;
          }
          .dataTables_paginate .paginate_button.disabled > a {
            background-color: #005eb8 !important;
            border-color:     #005eb8 !important;
            color:            rgba(255,255,255,0.45) !important;
          }

          /* ── Sidebar & layout ─────────────────────────────────────── */
          .sidebar-panel { background:#f8f9fa; border-radius:6px; padding:14px; }
          .well          { background:#f8f9fa; }
          h5             { margin-top:6px; }
        "))
      )
    ),
    menu_list
  )
)
