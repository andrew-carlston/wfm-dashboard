# ── Tab 1: Live Agent Status ──────────────────────────────────
# Shows all currently logged-in agents across AWS Connect and Five9.

library(shiny)
library(bs4Dash)
library(reactable)
library(dplyr)
library(lubridate)
library(jsonlite)

source("R/data_supabase.R")

# ── Status color mapping ─────────────────────────────────────
status_color <- function(status) {
  case_when(
    status %in% c("Available", "Ready")       ~ "#28a745",
    status %in% c("On Call", "On call")        ~ "#007bff",
    status == "ACW"                            ~ "#ffc107",
    status %in% c("Break", "Lunch")            ~ "#fd7e14",
    status %in% c("Not Ready", "Offline")      ~ "#dc3545",
    TRUE                                       ~ "#6c757d"
  )
}

# ── UI ───────────────────────────────────────────────────────
live_status_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
    fluidRow(
      column(12,
        bs4Card(
          title = "Live Agent Status",
          status = "primary",
          width = 12,
          solidHeader = TRUE,
          reactableOutput(ns("agent_table"))
        )
      )
    ),
    fluidRow(
      column(4, bs4ValueBoxOutput(ns("total_agents"), width = 12)),
      column(4, bs4ValueBoxOutput(ns("available_agents"), width = 12)),
      column(4, bs4ValueBoxOutput(ns("on_call_agents"), width = 12))
    )
  )
}

# ── Server ───────────────────────────────────────────────────
live_status_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Auto-refresh every 60 seconds
    auto_timer <- reactiveTimer(60000)

    # Reactive data: read latest snapshots from both platforms
    live_data <- reactive({
      auto_timer()
      input$refresh_btn  # also refresh on button click

      aws  <- tryCatch(read_latest_aws(), error = function(e) data.frame())
      five <- tryCatch(read_latest_five9(), error = function(e) data.frame())

      df <- bind_rows(aws, five)
      if (nrow(df) == 0) return(data.frame())

      df %>%
        mutate(
          duration_fmt = {
            dur <- as.integer(status_duration)
            sprintf("%02d:%02d", dur %/% 60, dur %% 60)
          },
          contact_state = sapply(contacts, function(c) {
            if (is.na(c) || c == "" || c == "null") return("")
            parsed <- tryCatch(fromJSON(c, simplifyVector = FALSE), error = function(e) list())
            if (length(parsed) == 0) return("")
            paste(sapply(parsed, function(x) {
              st <- if (!is.null(x[["state"]])) x[["state"]] else x[["AgentContactState"]]
              ch <- if (!is.null(x[["channel"]])) x[["channel"]] else x[["Channel"]]
              if (is.null(st)) return("")
              paste0(st, if (!is.null(ch)) paste0(" (", ch, ")") else "")
            }), collapse = ", ")
          })
        )
    })

    # Update filter choices when data changes
    observeEvent(live_data(), {
      df <- live_data()
      if (nrow(df) == 0) return()

      statuses <- sort(unique(df$status_name))
      updateSelectInput(session, "status_filter",
        choices = c("All", statuses), selected = input$status_filter)

      profiles <- sort(unique(na.omit(df$routing_profile)))
      updateSelectInput(session, "routing_filter",
        choices = c("All", profiles), selected = input$routing_filter)
    })

    # Filtered data
    filtered_data <- reactive({
      df <- live_data()
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

    # Agent table
    output$agent_table <- renderReactable({
      df <- filtered_data()
      if (nrow(df) == 0) {
        return(reactable(data.frame(Message = "No agents found"), columns = list(
          Message = colDef(align = "center")
        )))
      }

      display_df <- df %>%
        select(
          `Agent Email` = agent_email,
          Platform = platform,
          Status = status_name,
          Duration = duration_fmt,
          `Routing Profile` = routing_profile,
          `Contact State` = contact_state
        )

      reactable(
        display_df,
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        defaultPageSize = 50,
        columns = list(
          Status = colDef(
            cell = function(value) {
              color <- status_color(value)
              div(
                style = paste0(
                  "display: inline-block; padding: 2px 8px; border-radius: 4px; ",
                  "background-color: ", color, "; color: white; font-weight: bold;"
                ),
                value
              )
            }
          ),
          Duration = colDef(align = "right"),
          `Routing Profile` = colDef(na = "-"),
          `Contact State` = colDef(na = "")
        )
      )
    })

    # Value boxes
    output$total_agents <- renderbs4ValueBox({
      n <- nrow(filtered_data())
      bs4ValueBox(value = n, subtitle = "Total Agents", icon = icon("users"), color = "info")
    })

    output$available_agents <- renderbs4ValueBox({
      df <- filtered_data()
      n <- if (nrow(df) > 0) sum(df$status_name %in% c("Available", "Ready")) else 0
      bs4ValueBox(value = n, subtitle = "Available", icon = icon("check-circle"), color = "success")
    })

    output$on_call_agents <- renderbs4ValueBox({
      df <- filtered_data()
      n <- if (nrow(df) > 0) sum(df$status_name %in% c("On Call", "On call")) else 0
      bs4ValueBox(value = n, subtitle = "On Call", icon = icon("phone"), color = "primary")
    })

    # Last updated timestamp
    output$last_updated <- renderText({
      auto_timer()
      paste("Updated:", format(Sys.time(), "%H:%M:%S"))
    })
  })
}
