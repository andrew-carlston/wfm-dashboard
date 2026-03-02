# ── Tab 2: 15-Min Interval Grid ───────────────────────────────
# Time allocation per agent per 15-min slot with heatmap coloring.

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
      column(3, dateInput(ns("selected_date"), "Date", value = Sys.Date())),
      column(3, selectInput(ns("platform_filter"), "Platform",
        choices = c("All", "AWS", "Five9"), selected = "All")),
      column(3, selectInput(ns("agent_filter"), "Agent",
        choices = c("All"), selected = "All")),
      column(3, div(style = "padding-top: 25px;",
        actionButton(ns("load_btn"), "Load Data", icon = icon("table"),
                     class = "btn-primary btn-sm")
      ))
    ),
    fluidRow(
      column(12,
        bs4Card(
          title = "15-Minute Interval Grid",
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
interval_grid_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: load and process snapshots for selected date
    grid_data <- eventReactive(input$load_btn, {
      selected <- input$selected_date

      # Calculate hours_back from selected date to now
      hours_back <- as.numeric(difftime(Sys.time(),
        as.POSIXct(paste(selected, "00:00:00"), tz = "America/Chicago"),
        units = "hours")) + 24
      hours_back <- max(hours_back, 24)

      # Read snapshots
      aws  <- tryCatch(read_aws_snapshots(hours_back), error = function(e) data.frame())
      five <- tryCatch(read_five9_snapshots(hours_back), error = function(e) data.frame())
      all_snaps <- bind_rows(aws, five)

      if (nrow(all_snaps) == 0) return(list(grid = data.frame(), summary = data.frame()))

      # Platform filter
      if (input$platform_filter != "All") {
        all_snaps <- all_snaps %>% filter(platform == input$platform_filter)
      }

      # Build spans and convert to intervals
      spans <- build_spans(all_snaps)
      if (nrow(spans) == 0) return(list(grid = data.frame(), summary = data.frame()))

      intervals <- spans_to_intervals(spans)
      if (nrow(intervals) == 0) return(list(grid = data.frame(), summary = data.frame()))

      # Filter to selected date
      intervals <- intervals %>% filter(date == selected)

      # Agent summary: counts per state per interval
      state_cols <- setdiff(names(intervals), c("agent_email", "platform", "date", "interval"))
      summary_df <- intervals %>%
        group_by(interval) %>%
        summarise(across(all_of(state_cols), ~ sum(. > 0)), .groups = "drop") %>%
        arrange(interval)

      list(grid = intervals, summary = summary_df)
    }, ignoreNULL = FALSE)

    # Update agent filter when data loads
    observeEvent(grid_data(), {
      df <- grid_data()$grid
      if (nrow(df) == 0) return()
      agents <- sort(unique(df$agent_email))
      updateSelectInput(session, "agent_filter",
        choices = c("All", agents), selected = "All")
    })

    # Grid table
    output$grid_table <- renderReactable({
      gd <- grid_data()
      df <- gd$grid
      if (nrow(df) == 0) {
        return(reactable(data.frame(Message = "No data. Click 'Load Data'."),
          columns = list(Message = colDef(align = "center"))))
      }

      # Apply agent filter
      if (!is.null(input$agent_filter) && input$agent_filter != "All") {
        df <- df %>% filter(agent_email == input$agent_filter)
      }

      # Convert seconds to minutes for display
      state_cols <- setdiff(names(df), c("agent_email", "platform", "date", "interval"))

      display_df <- df %>%
        mutate(across(all_of(state_cols), ~ round(. / 60, 1))) %>%
        select(Agent = agent_email, Platform = platform, Interval = interval,
               all_of(state_cols))

      # Build column defs with heatmap coloring
      col_defs <- list(
        Agent = colDef(sticky = "left", style = list(fontWeight = "bold")),
        Platform = colDef(width = 80),
        Interval = colDef(width = 80)
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

      reactable(
        df,
        striped = TRUE,
        compact = TRUE,
        defaultPageSize = 96,
        columns = list(
          interval = colDef(name = "Interval", width = 80)
        )
      )
    })
  })
}
