# app/modules/pair_module.R


# ── UI ─────────────────────────────────────────────────────────────────────────

pair_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      width = 3,

      tags$h6("OFFENCE GROUP", class = "text-muted fw-bold mb-1 mt-1",
              style = "font-size:0.7rem; letter-spacing:0.05em;"),
      uiOutput(ns("main_group_ui")),

      tags$h6("SUBGROUP", class = "text-muted fw-bold mb-1",
              style = "font-size:0.7rem; letter-spacing:0.05em;"),
      uiOutput(ns("subgroup_ui")),

      tags$h6("FINANCIAL YEAR", class = "text-muted fw-bold mb-1",
              style = "font-size:0.7rem; letter-spacing:0.05em;"),
      uiOutput(ns("fy_ui")),

      uiOutput(ns("extra_filters")),

      hr(style = "margin:10px 0;"),
      actionButton(ns("reset"), "Reset all filters",
                   icon  = icon("undo"),
                   class = "btn btn-sm btn-outline-secondary w-100")
    ),

    mainPanel(
      width = 9,

      uiOutput(ns("panel_header")),

      fluidRow(
        column(6, h5(strong("Open QPS Data")),
               plotlyOutput(ns("chart_real"), height = "400px")),
        column(6, h5(strong("CS Data")),
               plotlyOutput(ns("chart_mock"), height = "400px"))
      ),

      hr(),
      h5(strong("Comparison")),
      tabsetPanel(
        id = ns("comp_tabs"),
        tabPanel("By Subgroup", br(), DTOutput(ns("tbl_subgroup"))),
        tabPanel("By Offence",  br(), DTOutput(ns("tbl_offence"))),
        tabPanel("By Month",    br(), DTOutput(ns("tbl_monthly")))
      )
    )
  )
}


# ── Server ─────────────────────────────────────────────────────────────────────

pair_server <- function(id, pair_data = NULL, metric = "Number") {
  moduleServer(id, function(input, output, session) {

    ns      <- session$ns
    safe_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

    ds_info  <- DATASET_INFO[name == id]
    ds_label <- if (nrow(ds_info) > 0L) ds_info$label[1L] else id
    ds_group <- if (nrow(ds_info) > 0L) ds_info$group[1L] else ""

    get_data <- reactive({
      if (is.null(pair_data)) load_pair(id) else pair_data
    })

    qps  <- reactive(get_data()$qps)
    cs   <- reactive(get_data()$cs)

    extra_cols <- reactive({
      d    <- qps()
      geo  <- Filter(nchar, strsplit(d[1L, geo_cols],  "\\|")[[1L]])
      demo <- Filter(nchar, strsplit(d[1L, demo_cols], "\\|")[[1L]])
      c(geo, demo)
    })

    fy_vals <- reactive(sort(unique(qps()$financial_year)))

    current_fy <- local({
      m <- as.integer(format(Sys.Date(), "%m"))
      y <- as.integer(format(Sys.Date(), "%Y"))
      if (m >= 7L) paste0(y, "-", y + 1L) else paste0(y - 1L, "-", y)
    })

    fy_default <- reactive({
      if (current_fy %in% fy_vals()) current_fy else tail(fy_vals(), 1L)
    })

    # Available main groups derived from the actual data for this dataset
    avail_groups <- reactive({
      sort(unique(qps()[!(is_subtotal), main_group]))
    })

    # When a single subgroup is selected, charts drill down to offence level
    fill_col <- reactive({
      sg <- input$subgroup
      if (!is.null(sg) && !"All" %in% sg && length(sg) == 1L) "offence"
      else "subgroup"
    })


    # ── Panel header -----------------------------------------------------------

    output$panel_header <- renderUI({
      level_str <- switch(ds_label,
        "QLD"      = "Queensland-wide",
        "Region"   = "By Police Region",
        "District" = "By Police District",
        "Division" = "By Police Division",
        "LGA"      = "By Local Government Area",
        ds_label
      )
      demo <- Filter(nchar, strsplit(qps()[1L, demo_cols], "\\|")[[1L]])
      if (length(demo) > 0L)
        level_str <- paste0(level_str, " · by ",
                            paste(demo, collapse = " & "))
      tags$div(
        class = "mb-3 pb-2",
        style = "border-bottom:3px solid #78003F;",
        tags$span(ds_group,
                  style = "color:#78003F; font-weight:700; font-size:1rem;"),
        tags$span(paste0(" — ", level_str),
                  style = "color:#555; font-size:0.95rem;")
      )
    })


    # ── Lazy selector UI -------------------------------------------------------

    # Only render the groups that exist in this dataset — Victims tabs will
    # therefore only show "Offences Against the Person".
    output$main_group_ui <- renderUI({
      grps <- avail_groups()
      checkboxGroupInput(
        ns("main_group"), label = NULL,
        choices  = grps,
        selected = grps
      )
    })

    output$fy_ui <- renderUI({
      selectInput(ns("fy"), label = NULL,
                  choices   = rev(fy_vals()),
                  selected  = fy_default(),
                  multiple  = TRUE,
                  selectize = TRUE)
    })

    output$subgroup_ui <- renderUI({
      sg <- if (length(input$main_group) > 0L)
        sort(unique(qps()[main_group %in% input$main_group &
                            !(is_subtotal), subgroup]))
      else character(0L)
      selectInput(ns("subgroup"), label = NULL,
                  choices   = c("All" = "All", setNames(sg, sg)),
                  selected  = "All",
                  multiple  = TRUE,
                  selectize = TRUE)
    })

    # ── "All" auto-removal ─────────────────────────────────────────────────────
    # Rule: when "All" and specific items are selected together,
    #   • if "All" is newly added → keep only "All" (clear specifics)
    #   • if specifics are newly added → drop "All" (keep specifics)

    filter_history <- new.env(parent = emptyenv())
    extra_obs_done <- reactiveVal(FALSE)

    resolve_all <- function(sel, prev) {
      if (length(sel) > 1L && "All" %in% sel) {
        if (is.null(prev) || !"All" %in% prev) "All" else sel[sel != "All"]
      } else {
        sel
      }
    }

    observeEvent(input$subgroup, {
      sel  <- input$subgroup
      prev <- if (exists("subgroup", envir = filter_history, inherits = FALSE))
                get("subgroup", envir = filter_history, inherits = FALSE)
              else NULL
      new_sel <- resolve_all(sel, prev)
      if (!identical(new_sel, sel))
        updateSelectInput(session, "subgroup", selected = new_sel)
      assign("subgroup", new_sel, envir = filter_history)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    output$extra_filters <- renderUI({
      cols <- extra_cols()
      if (length(cols) == 0L) return(NULL)

      # Register per-column All-removal observers once, on first render.
      # isolate() keeps the reactiveVal write from re-triggering this renderUI.
      if (isFALSE(isolate(extra_obs_done()))) {
        isolate(extra_obs_done(TRUE))
        lapply(cols, function(col) {
          local({
            input_id <- paste0("f_", safe_id(col))
            observeEvent(input[[input_id]], {
              sel  <- input[[input_id]]
              prev <- if (exists(input_id, envir = filter_history, inherits = FALSE))
                        get(input_id, envir = filter_history, inherits = FALSE)
                      else NULL
              new_sel <- resolve_all(sel, prev)
              if (!identical(new_sel, sel))
                updateSelectInput(session, input_id, selected = new_sel)
              assign(input_id, new_sel, envir = filter_history)
            }, ignoreNULL = TRUE, ignoreInit = TRUE)
          })
        })
      }

      r <- qps()
      tagList(lapply(cols, function(col) {
        vals <- sort(unique(r[[col]]))
        tagList(
          tags$h6(toupper(col), class = "text-muted fw-bold mb-1",
                  style = "font-size:0.7rem; letter-spacing:0.05em;"),
          selectInput(
            ns(paste0("f_", safe_id(col))), label = NULL,
            choices   = c("(All)" = "All", setNames(vals, vals)),
            selected  = "All", multiple = TRUE, selectize = TRUE
          )
        )
      }))
    })


    # ── Reset ------------------------------------------------------------------

    observeEvent(input$reset, {
      grps <- isolate(avail_groups())
      updateCheckboxGroupInput(session, "main_group", selected = grps)
      updateSelectInput(session, "fy",       selected = isolate(fy_default()))
      updateSelectInput(session, "subgroup", selected = "All")
      for (col in isolate(extra_cols()))
        updateSelectInput(session, paste0("f_", safe_id(col)), selected = "All")
      rm(list = ls(filter_history), envir = filter_history)
    })


    # ── Filter helper ----------------------------------------------------------

    filter_dt <- function(dt) {
      res <- dt[is_subtotal == FALSE]

      fy_sel <- input$fy
      if (length(fy_sel) > 0L) res <- res[financial_year %in% fy_sel]

      mg <- input$main_group
      if (length(mg) > 0L) res <- res[main_group %in% mg]

      sg <- input$subgroup
      if (!is.null(sg) && !"All" %in% sg && length(sg) > 0L)
        res <- res[subgroup %in% sg]

      for (col in extra_cols()) {
        val <- input[[paste0("f_", safe_id(col))]]
        if (!is.null(val) && !"All" %in% val && length(val) > 0L)
          res <- res[get(col) %in% val]
      }
      res
    }

    filt_qps <- reactive(filter_dt(qps()))
    filt_cs  <- reactive(filter_dt(cs()))


    # ── Interactive bar chart --------------------------------------------------

    y_lab <- if (metric == "Rate") "Rate per 100,000 population" else "Count"

    make_chart <- function(dt, title, fc, y_max) {
      if (nrow(dt) == 0L) {
        return(
          plot_ly() %>%
            add_annotations(
              text      = "No data matches the current filters",
              x = 0.5, y = 0.5, showarrow = FALSE,
              font      = list(size = 14, color = "grey50"),
              xref = "paper", yref = "paper"
            ) %>%
            layout(xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE))
        )
      }

      pd <- dt[, .(count = sum(count, na.rm = TRUE)),
               by = c("date", "financial_year", fc)]
      setorder(pd, date)

      if (fc == "subgroup") {
        vals   <- sort(unique(pd$subgroup))
        colors <- SUBGROUP_COLORS[vals]
        colors[is.na(colors)] <- "#CCCCCC"
        colors <- setNames(colors, vals)
      } else {
        parent_col <- SUBGROUP_COLORS[unique(dt$subgroup)[1L]]
        if (is.na(parent_col)) parent_col <- "#78003F"
        vals   <- sort(unique(pd$offence))
        n      <- length(vals)
        colors <- setNames(
          colorRampPalette(c(parent_col, "#D8D8D8"))(n + 1L)[seq_len(n)],
          vals
        )
      }

      pd[, tip := paste0("<b>", get(fc), "</b><br>",
                          format(date, "%B %Y"), "<br>",
                          y_lab, ": ", format(round(count), big.mark = ","))]

      p <- ggplot(pd, aes(x = date, y = count,
                           fill = .data[[fc]], text = tip)) +
        geom_col(width = 20) +
        scale_x_date(date_labels = "%b %y", date_breaks = "2 months") +
        scale_y_continuous(labels = comma,
                           limits = c(0, y_max),
                           expand = expansion(mult = c(0, 0))) +
        scale_fill_manual(values = colors, drop = TRUE, na.value = "#CCCCCC") +
        labs(x = NULL, y = y_lab, fill = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
          legend.position    = "bottom",
          legend.text        = element_text(size = 8),
          legend.key.size    = unit(0.4, "cm"),
          panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text         = element_text(face = "bold", colour = "#005DA6")
        )

      n_fy <- uniqueN(pd$financial_year)
      if (n_fy > 1L)
        p <- p + facet_wrap(~financial_year, nrow = 1L, scales = "free_x")
      else
        p <- p + labs(caption = paste("FY", unique(pd$financial_year)))

      ggplotly(p, tooltip = "text") %>%
        layout(
          title  = list(text  = paste0("<b>", title, "</b>"),
                        font  = list(color = "#78003F", size = 13),
                        x = 0, xref = "paper"),
          legend = list(orientation = "h", xanchor = "center",
                        x = 0.5, y = -0.25, font = list(size = 10)),
          margin = list(t = 50)
        ) %>%
        config(displayModeBar = FALSE)
    }

    shared_y_max <- reactive({
      fc <- fill_col()
      tot <- function(dt) dt[, .(total = sum(count, na.rm = TRUE)),
                              by = c("date", "financial_year")][, max(total, na.rm = TRUE)]
      max(tot(filt_qps()), tot(filt_cs()), na.rm = TRUE) * 1.12
    })

    output$chart_real <- renderPlotly(make_chart(filt_qps(), "Open QPS Data", fill_col(), shared_y_max()))
    output$chart_mock <- renderPlotly(make_chart(filt_cs(),  "CS Data",       fill_col(), shared_y_max()))


    # ── Comparison tables ------------------------------------------------------

    comp_subgroup <- reactive({
      q <- filt_qps()[, .(qps = sum(count, na.rm = TRUE)), by = subgroup]
      c <- filt_cs()[ , .(cs  = sum(count, na.rm = TRUE)), by = subgroup]
      comp <- merge(q, c, by = "subgroup", all = TRUE)
      comp[is.na(qps), qps := 0L][is.na(cs), cs := 0L]
      comp[, diff     := cs - qps]
      comp[, pct_diff := fifelse(qps > 0L,
                                  round((cs - qps) / qps * 100, 1L),
                                  NA_real_)]
      setorder(comp, subgroup)
      total <- data.table(subgroup = "TOTAL",
                          qps = comp[, sum(qps)], cs = comp[, sum(cs)])
      total[, diff    := cs - qps]
      total[, pct_diff := round((cs - qps) / qps * 100, 1L)]
      rbindlist(list(comp, total), use.names = TRUE)
    })

    comp_offence <- reactive({
      q <- filt_qps()[, .(qps = sum(count, na.rm = TRUE)),
                      by = .(offence, subgroup)]
      c <- filt_cs()[ , .(cs  = sum(count, na.rm = TRUE)),
                      by = .(offence, subgroup)]
      comp <- merge(q, c, by = c("offence", "subgroup"), all = TRUE)
      comp[is.na(qps), qps := 0L][is.na(cs), cs := 0L]
      comp[, diff     := cs - qps]
      comp[, pct_diff := fifelse(qps > 0L,
                                  round((cs - qps) / qps * 100, 1L),
                                  NA_real_)]
      setorder(comp, subgroup, offence)
      comp[, .(offence, subgroup, qps, cs, diff, pct_diff)]
    })

    comp_monthly <- reactive({
      q <- filt_qps()[, .(qps = sum(count, na.rm = TRUE)),
                      by = .(date, financial_year)]
      c <- filt_cs()[ , .(cs  = sum(count, na.rm = TRUE)),
                      by = .(date, financial_year)]
      comp <- merge(q, c, by = c("date", "financial_year"), all = TRUE)
      comp[is.na(qps), qps := 0L][is.na(cs), cs := 0L]
      comp[, diff     := cs - qps]
      comp[, pct_diff := fifelse(qps > 0L,
                                  round((cs - qps) / qps * 100, 1L),
                                  NA_real_)]
      comp[, month := format(date, "%b %Y")]
      setorder(comp, date)
      comp[, .(month, financial_year, qps, cs, diff, pct_diff)]
    })

    render_comp <- function(dt, col_names, num_idx, pct_idx) {
      tbl <- copy(dt)
      setnames(tbl, col_names)
      datatable(tbl, rownames = FALSE,
                class   = "compact stripe hover",
                options = list(pageLength = 25, dom = "tip", scrollX = TRUE)) |>
        formatRound(col_names[num_idx], digits = 0) |>
        formatStyle(col_names[pct_idx],
                    color = styleInterval(c(-0.001, 0.001),
                                          c("#CB2B3B", "#555555", "#489A4A")))
    }

    output$tbl_subgroup <- renderDT(render_comp(
      comp_subgroup(),
      col_names = c("Subgroup", "QPS", "CS", "Difference", "% Diff"),
      num_idx = 2:4, pct_idx = 5L
    ))

    output$tbl_offence <- renderDT(render_comp(
      comp_offence(),
      col_names = c("Offence", "Subgroup", "QPS", "CS", "Difference", "% Diff"),
      num_idx = 3:5, pct_idx = 6L
    ))

    output$tbl_monthly <- renderDT(render_comp(
      comp_monthly(),
      col_names = c("Month", "Financial Year", "QPS", "CS", "Difference", "% Diff"),
      num_idx = 3:5, pct_idx = 6L
    ))

  })
}
