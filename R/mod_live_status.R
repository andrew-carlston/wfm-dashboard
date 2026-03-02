# ── Tab 1: Live Agent Status ──────────────────────────────────
# One card per status, each containing a table of agents in that state.

library(shiny)
library(bs4Dash)
library(reactable)
library(dplyr)
library(lubridate)

source("R/data_supabase.R")

# ── Status config ────────────────────────────────────────────
STATUS_CONFIG <- list(
  Available   = list(bg = "#28a745", icon = "fa-check-circle",   order = 1),
  `On Call`   = list(bg = "#007bff", icon = "fa-phone-alt",      order = 2),
  ACW         = list(bg = "#ffc107", icon = "fa-clipboard",      order = 3),
  Break       = list(bg = "#fd7e14", icon = "fa-mug-hot",        order = 4),
  Lunch       = list(bg = "#e83e8c", icon = "fa-utensils",       order = 5),
  Training    = list(bg = "#6f42c1", icon = "fa-graduation-cap", order = 6),
  Meeting     = list(bg = "#20c997", icon = "fa-calendar",       order = 7),
  Coaching    = list(bg = "#17a2b8", icon = "fa-chalkboard-teacher", order = 8),
  `Not Ready` = list(bg = "#dc3545", icon = "fa-times-circle",   order = 9),
  Offline     = list(bg = "#6c757d", icon = "fa-power-off",      order = 10)
)

get_config <- function(status) {
  if (status %in% names(STATUS_CONFIG)) STATUS_CONFIG[[status]]
  else list(bg = "#6c757d", icon = "fa-question-circle", order = 99)
}

# ── Format duration as hh:mm:ss ──────────────────────────────
fmt_duration <- function(secs) {
  secs <- as.integer(secs)
  hrs <- secs %/% 3600
  mins <- (secs %% 3600) %/% 60
  s <- secs %% 60
  sprintf("%02d:%02d:%02d", hrs, mins, s)
}

# ── UI ───────────────────────────────────────────────────────
live_status_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      .status-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(380px, 1fr));
        gap: 16px;
        padding: 0 4px;
      }
      .status-card {
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 2px 8px rgba(0,0,0,0.12);
        background: #fff;
      }
      .status-card-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 10px 16px;
        color: #fff;
        font-weight: 600;
      }
      .status-card-header .status-label {
        font-size: 15px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .status-card-header .status-count {
        font-size: 22px;
        font-weight: 700;
      }
      .status-card-body {
        max-height: 280px;
        overflow-y: auto;
      }
      .status-card-body table {
        width: 100%;
        margin: 0;
        font-size: 13px;
      }
      .status-card-body table th {
        position: sticky;
        top: 0;
        background: #f8f9fa;
        padding: 6px 12px;
        font-weight: 600;
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        color: #666;
        border-bottom: 2px solid #dee2e6;
      }
      .status-card-body table td {
        padding: 5px 12px;
        border-bottom: 1px solid #f0f0f0;
        white-space: nowrap;
      }
      .status-card-body table tr:hover {
        background: #f8f9fa;
      }
      .status-card-body table td.duration {
        font-family: 'SF Mono', 'Consolas', 'Monaco', monospace;
        text-align: right;
        font-weight: 500;
      }
      .platform-badge {
        display: inline-block;
        padding: 1px 6px;
        border-radius: 3px;
        font-size: 10px;
        font-weight: 600;
        color: #fff;
      }
      .platform-aws { background: #232f3e; }
      .platform-five9 { background: #ff6a00; }
      .summary-bar {
        display: flex;
        flex-wrap: wrap;
        gap: 0;
        margin-bottom: 16px;
        background: #fff;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        overflow: hidden;
      }
      .summary-chip {
        flex: 1 1 0;
        min-width: 80px;
        text-align: center;
        padding: 12px 8px;
        color: #fff;
        border-right: 1px solid rgba(255,255,255,0.15);
      }
      .summary-chip:last-child { border-right: none; }
      .summary-chip .chip-count { font-size: 24px; font-weight: 700; line-height: 1; }
      .summary-chip .chip-label { font-size: 10px; text-transform: uppercase; letter-spacing: 0.5px; opacity: 0.9; margin-top: 2px; }
      .spinner-wrap {
        display: flex;
        justify-content: center;
        align-items: center;
        padding: 80px 0;
        flex-direction: column;
        gap: 12px;
        color: #999;
      }
      .css-spinner {
        width: 40px;
        height: 40px;
        border: 4px solid #e9ecef;
        border-top: 4px solid #007bff;
        border-radius: 50%;
        animation: spin 0.8s linear infinite;
      }
      @keyframes spin {
        to { transform: rotate(360deg); }
      }
    "))),
    fluidRow(
      column(3, selectInput(ns("platform_filter"), "Platform",
        choices = c("All", "AWS", "Five9"), selected = "All")),
      column(3, selectInput(ns("status_filter"), "Status",
        choices = c("All"), selected = "All")),
      column(3, selectInput(ns("routing_filter"), "Routing Profile",
        choices = c("All"), selected = "All")),
      column(3, div(style = "padding-top: 25px;",
        actionButton(ns("refresh_btn"), "Refresh Now", icon = icon("sync"),
                     class = "btn-primary btn-sm"),
        textOutput(ns("last_updated"), inline = TRUE)
      ))
    ),
    # Summary bar
    uiOutput(ns("summary_bar")),
    # Status cards grid
    uiOutput(ns("status_cards"))
  )
}

# ── Server ───────────────────────────────────────────────────
live_status_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    auto_timer <- reactiveTimer(60000)
    first_load <- reactiveVal(TRUE)

    # Cache: holds last good dataset so UI never blanks during refresh
    cached_data <- reactiveVal(data.frame())

    # Fetch new data silently — update cache only when ready
    observe({
      auto_timer()
      input$refresh_btn

      aws  <- tryCatch(read_latest_aws(), error = function(e) data.frame())
      five <- tryCatch(read_latest_five9(), error = function(e) data.frame())

      df <- bind_rows(aws, five)
      if (nrow(df) > 0) {
        df <- df %>% mutate(duration_fmt = fmt_duration(status_duration))
      }

      cached_data(df)
      first_load(FALSE)
    })

    # Update filter choices
    observeEvent(cached_data(), {
      df <- cached_data()
      if (nrow(df) == 0) return()

      statuses <- sort(unique(df$status_name))
      updateSelectInput(session, "status_filter",
        choices = c("All", statuses), selected = input$status_filter)

      profiles <- sort(unique(na.omit(df$routing_profile)))
      updateSelectInput(session, "routing_filter",
        choices = c("All", profiles), selected = input$routing_filter)
    })

    # Filtered data (reads from cache — always instant)
    filtered_data <- reactive({
      df <- cached_data()
      if (nrow(df) == 0) return(data.frame())

      if (input$platform_filter != "All") {
        df <- df %>% filter(platform == input$platform_filter)
      }
      if (!is.null(input$status_filter) && input$status_filter != "All") {
        df <- df %>% filter(status_name == input$status_filter)
      }
      if (!is.null(input$routing_filter) && input$routing_filter != "All") {
        df <- df %>% filter(routing_profile == input$routing_filter)
      }
      df
    })

    # Summary bar
    output$summary_bar <- renderUI({
      if (first_load()) return(NULL)
      df <- filtered_data()
      if (nrow(df) == 0) return(NULL)

      counts <- df %>%
        count(status_name, name = "n") %>%
        mutate(order = sapply(status_name, function(s) get_config(s)$order)) %>%
        arrange(order)

      chips <- list(
        tags$div(class = "summary-chip", style = "background: #343a40;",
          tags$div(class = "chip-count", nrow(df)),
          tags$div(class = "chip-label", "Total")
        )
      )

      for (i in seq_len(nrow(counts))) {
        cfg <- get_config(counts$status_name[i])
        chips[[length(chips) + 1]] <- tags$div(
          class = "summary-chip",
          style = paste0("background: ", cfg$bg, ";"),
          tags$div(class = "chip-count", counts$n[i]),
          tags$div(class = "chip-label", counts$status_name[i])
        )
      }

      tags$div(class = "summary-bar", do.call(tagList, chips))
    })

    # Status cards — one card per status with agent table inside
    output$status_cards <- renderUI({
      if (first_load()) {
        return(tags$div(class = "spinner-wrap",
          tags$div(class = "css-spinner"),
          tags$div("Loading agent data...")
        ))
      }

      df <- filtered_data()
      if (nrow(df) == 0) {
        return(tags$div(class = "spinner-wrap",
          tags$i(class = "fas fa-users", style = "font-size: 40px;"),
          tags$div("No agents online")
        ))
      }

      # Split by status, ordered
      df <- df %>%
        mutate(status_order = sapply(status_name, function(s) get_config(s)$order)) %>%
        arrange(status_order, agent_email)

      statuses <- df %>%
        group_by(status_name) %>%
        summarise(n = n(), order = first(status_order), .groups = "drop") %>%
        arrange(order)

      cards <- lapply(seq_len(nrow(statuses)), function(i) {
        status <- statuses$status_name[i]
        count <- statuses$n[i]
        cfg <- get_config(status)

        agents <- df %>%
          filter(status_name == status) %>%
          arrange(desc(status_duration))

        # Build table rows
        rows <- lapply(seq_len(nrow(agents)), function(j) {
          a <- agents[j, ]
          platform_class <- if (a$platform == "AWS") "platform-aws" else "platform-five9"
          tags$tr(
            tags$td(a$agent_email),
            tags$td(tags$span(class = paste("platform-badge", platform_class), a$platform)),
            tags$td(class = "duration", a$duration_fmt),
            tags$td(if (!is.na(a$routing_profile)) a$routing_profile else "-")
          )
        })

        tags$div(class = "status-card",
          tags$div(class = "status-card-header", style = paste0("background: ", cfg$bg, ";"),
            tags$span(class = "status-label",
              tags$i(class = paste("fas", cfg$icon)),
              status
            ),
            tags$span(class = "status-count", count)
          ),
          tags$div(class = "status-card-body",
            tags$table(
              tags$thead(tags$tr(
                tags$th("Agent"),
                tags$th("Platform"),
                tags$th(style = "text-align: right;", "Duration"),
                tags$th("Routing Profile")
              )),
              tags$tbody(rows)
            )
          )
        )
      })

      tags$div(class = "status-grid", do.call(tagList, cards))
    })

    # Last updated
    output$last_updated <- renderText({
      auto_timer()
      paste("Updated:", format(Sys.time(), "%H:%M:%S"))
    })
  })
}
