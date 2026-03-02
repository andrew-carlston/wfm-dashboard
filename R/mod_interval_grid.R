# ── Tab 2: 15-Min Interval Grid ───────────────────────────────
# Today's time allocation per agent per 15-min slot.
# Auto-refreshes every 60s. Handles overnight shifts.

library(shiny)
library(bs4Dash)
library(reactable)
library(dplyr)
library(tidyr)
library(lubridate)

source("R/data_supabase.R")
source("R/data_grid.R")

# ── UI ───────────────────────────────────────────────────────
interval_grid_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("platform_filter"), "Platform",
        choices = c("All", "AWS", "Five9"), selected = "All")),
      column(4, selectInput(ns("agent_filter"), "Agent",
        choices = c("All"), selected = "All")),
      column(4, div(style = "padding-top: 25px;",
        actionButton(ns("load_btn"), "Refresh Now", icon = icon("sync"),
                     class = "btn-primary btn-sm"),
        textOutput(ns("last_updated"), inline = TRUE)
      ))
    ),
    fluidRow(
      column(12,
        bs4Card(
          title = paste("15-Minute Interval Grid —", format(Sys.Date(), "%A, %B %d")),
          status = "primary",
          width = 12,
          solidHeader = TRUE,
          div(style = "overflow-x: auto;",
            reactableOutput(ns("grid_table"))
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Card(
          title = "Summary: Agents per State per Interval",
          status = "info",
          width = 12,
          solidHeader = TRUE,
          collapsed = TRUE,
          reactableOutput(ns("summary_table"))
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
interval_grid_server <- function(id, active_tab = reactive("live")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track if tab has been visited
    tab_visited <- reactiveVal(FALSE)
    observeEvent(active_tab(), {
      if (active_tab() == "grid") tab_visited(TRUE)
    })

    auto_timer <- reactiveTimer(60000)

    # Only start loading after user visits this tab
    grid_data <- reactive({
      req(tab_visited())
      auto_timer()
      input$load_btn

      today <- Sys.Date()

      message("[GRID] Fetching snapshots (36hr)...")
      # 36 hours back captures overnight shifts
      aws  <- tryCatch(read_aws_snapshots(36), error = function(e) {
        message(paste("[GRID] AWS error:", e$message))
        data.frame()
      })
      five <- tryCatch(read_five9_snapshots(36), error = function(e) {
        message(paste("[GRID] Five9 error:", e$message))
        data.frame()
      })
      all_snaps <- bind_rows(aws, five)
      message(paste("[GRID] Total snapshots:", nrow(all_snaps)))

      if (nrow(all_snaps) == 0) return(list(grid = data.frame(), summary = data.frame()))

      if (input$platform_filter != "All") {
        all_snaps <- all_snaps %>% filter(platform == input$platform_filter)
      }

      spans <- build_spans(all_snaps)
      if (nrow(spans) == 0) return(list(grid = data.frame(), summary = data.frame()))

      intervals <- spans_to_intervals(spans)
      if (nrow(intervals) == 0) return(list(grid = data.frame(), summary = data.frame()))

      # Filter to today (overnight rollovers already split by split_midnight)
      intervals <- intervals %>% filter(date == today)
      if (nrow(intervals) == 0) return(list(grid = data.frame(), summary = data.frame()))

      state_cols <- setdiff(names(intervals), c("agent_email", "platform", "date", "interval"))
      summary_df <- intervals %>%
        group_by(interval) %>%
        summarise(across(all_of(state_cols), ~ sum(. > 0)), .groups = "drop") %>%
        arrange(interval)

      list(grid = intervals, summary = summary_df)
    })

    # Update agent filter
    observeEvent(grid_data(), {
      df <- grid_data()$grid
      if (nrow(df) == 0) return()
      agents <- sort(unique(df$agent_email))
      updateSelectInput(session, "agent_filter",
        choices = c("All", agents), selected = input$agent_filter)
    })

    # Grid table
    output$grid_table <- renderReactable({
      gd <- grid_data()
      df <- gd$grid
      if (nrow(df) == 0) {
        return(reactable(data.frame(Message = "No interval data for today."),
          columns = list(Message = colDef(align = "center"))))
      }

      if (!is.null(input$agent_filter) && input$agent_filter != "All") {
        df <- df %>% filter(agent_email == input$agent_filter)
      }

      state_cols <- setdiff(names(df), c("agent_email", "platform", "date", "interval"))

      display_df <- df %>%
        mutate(across(all_of(state_cols), ~ round(. / 60, 1))) %>%
        select(Agent = agent_email, Platform = platform, Interval = interval,
               all_of(state_cols))

      col_defs <- list(
        Agent = colDef(sticky = "left", style = list(fontWeight = "bold"), minWidth = 200),
        Platform = colDef(width = 80, align = "center"),
        Interval = colDef(width = 80, style = list(fontFamily = "monospace"))
      )

      for (col in state_cols) {
        col_defs[[col]] <- colDef(
          width = 80,
          align = "center",
          cell = function(value) {
            if (is.na(value) || value == 0) return("")
            sprintf("%.1f", value)
          },
          style = function(value) {
            if (is.na(value) || value == 0) return(list())
            intensity <- min(value / 15, 1)
            bg <- sprintf("rgba(0, 123, 255, %.2f)", intensity * 0.6)
            list(background = bg, fontWeight = "bold")
          }
        )
      }

      reactable(
        display_df,
        columns = col_defs,
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        defaultPageSize = 96,
        compact = TRUE
      )
    })

    # Summary table
    output$summary_table <- renderReactable({
      gd <- grid_data()
      df <- gd$summary
      if (nrow(df) == 0) {
        return(reactable(data.frame(Message = "No summary data."),
          columns = list(Message = colDef(align = "center"))))
      }

      reactable(df, striped = TRUE, compact = TRUE, defaultPageSize = 96,
        columns = list(interval = colDef(name = "Interval", width = 80)))
    })

    # Last updated
    output$last_updated <- renderText({
      auto_timer()
      paste("Updated:", format(Sys.time(), "%H:%M:%S"))
    })
  })
}
