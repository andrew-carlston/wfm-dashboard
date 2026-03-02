# ── Tab 1: Live Agent Status ──────────────────────────────────
# Shows all currently logged-in agents across AWS Connect and Five9.
# Status cards at top, agent table below.

library(shiny)
library(bs4Dash)
library(reactable)
library(dplyr)
library(lubridate)

source("R/data_supabase.R")

# ── Status color mapping ─────────────────────────────────────
STATUS_COLORS <- list(
  Available = "#28a745", Ready = "#28a745",
  `On Call` = "#007bff", `On call` = "#007bff",
  ACW = "#ffc107", `After Call Work` = "#ffc107",

  Break = "#fd7e14", Lunch = "#fd7e14",
  `Not Ready` = "#dc3545", Offline = "#dc3545"
)

STATUS_ICONS <- list(
  Available = "check-circle", Ready = "check-circle",
  `On Call` = "phone", `On call` = "phone",
  ACW = "clipboard", `After Call Work` = "clipboard",
  Break = "mug-hot", Lunch = "utensils",
  `Not Ready` = "times-circle", Offline = "times-circle"
)

get_status_color <- function(status) {
  if (status %in% names(STATUS_COLORS)) STATUS_COLORS[[status]] else "#6c757d"
}

get_status_icon <- function(status) {
  if (status %in% names(STATUS_ICONS)) STATUS_ICONS[[status]] else "question-circle"
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
    # Status summary cards
    fluidRow(
      uiOutput(ns("status_cards"))
    ),
    # Agent table
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
      input$refresh_btn

      aws  <- tryCatch(read_latest_aws(), error = function(e) data.frame())
      five <- tryCatch(read_latest_five9(), error = function(e) data.frame())

      df <- bind_rows(aws, five)
      if (nrow(df) == 0) return(data.frame())

      df %>%
        mutate(duration_fmt = fmt_duration(status_duration))
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

    # Status summary cards — one card per status
    output$status_cards <- renderUI({
      df <- filtered_data()
      if (nrow(df) == 0) return(NULL)

      counts <- df %>%
        count(status_name, name = "n") %>%
        arrange(desc(n))

      total <- nrow(df)

      # Total card first
      cards <- list(
        column(2,
          bs4ValueBox(
            value = total,
            subtitle = "Total",
            icon = icon("users"),
            color = "lightblue",
            width = 12
          )
        )
      )

      # One card per status
      for (i in seq_len(nrow(counts))) {
        status <- counts$status_name[i]
        n <- counts$n[i]
        color_hex <- get_status_color(status)
        ico <- get_status_icon(status)

        # Map hex to bs4Dash color names
        bs4_color <- switch(color_hex,
          "#28a745" = "success",
          "#007bff" = "primary",
          "#ffc107" = "warning",
          "#fd7e14" = "orange",
          "#dc3545" = "danger",
          "secondary"
        )

        cards[[length(cards) + 1]] <- column(2,
          bs4ValueBox(
            value = n,
            subtitle = status,
            icon = icon(ico),
            color = bs4_color,
            width = 12
          )
        )
      }

      do.call(tagList, cards)
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
          `Routing Profile` = routing_profile
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
              color <- get_status_color(value)
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
          `Routing Profile` = colDef(na = "-")
        )
      )
    })

    # Last updated timestamp
    output$last_updated <- renderText({
      auto_timer()
      paste("Updated:", format(Sys.time(), "%H:%M:%S"))
    })
  })
}
