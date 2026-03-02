# ── Tab 1: Live Agent Status ──────────────────────────────────
# Shows all currently logged-in agents across AWS Connect and Five9.
# Status summary cards at top, agent table below.

library(shiny)
library(bs4Dash)
library(reactable)
library(dplyr)
library(lubridate)

source("R/data_supabase.R")

# ── Status config ────────────────────────────────────────────
STATUS_CONFIG <- list(
  Total      = list(bg = "#343a40", icon = "fa-users"),
  Available  = list(bg = "#28a745", icon = "fa-check-circle"),
  `On Call`  = list(bg = "#007bff", icon = "fa-phone-alt"),
  ACW        = list(bg = "#ffc107", icon = "fa-clipboard"),
  Break      = list(bg = "#fd7e14", icon = "fa-mug-hot"),
  Lunch      = list(bg = "#e83e8c", icon = "fa-utensils"),
  `Not Ready` = list(bg = "#dc3545", icon = "fa-times-circle"),
  Offline    = list(bg = "#dc3545", icon = "fa-power-off"),
  Training   = list(bg = "#6f42c1", icon = "fa-graduation-cap"),
  Meeting    = list(bg = "#20c997", icon = "fa-calendar"),
  Coaching   = list(bg = "#17a2b8", icon = "fa-chalkboard-teacher")
)

get_config <- function(status) {
  if (status %in% names(STATUS_CONFIG)) STATUS_CONFIG[[status]]
  else list(bg = "#6c757d", icon = "fa-question-circle")
}

# ── Format duration as hh:mm:ss ──────────────────────────────
fmt_duration <- function(secs) {
  secs <- as.integer(secs)
  hrs <- secs %/% 3600
  mins <- (secs %% 3600) %/% 60
  s <- secs %% 60
  sprintf("%02d:%02d:%02d", hrs, mins, s)
}

# ── Build a single status card (raw HTML) ────────────────────
make_status_card <- function(label, count, bg, icon_class) {
  tags$div(
    style = paste0(
      "flex: 1 1 0; min-width: 120px; max-width: 180px; ",
      "background: ", bg, "; color: #fff; ",
      "border-radius: 8px; padding: 14px 16px; margin: 4px; ",
      "text-align: center; box-shadow: 0 2px 6px rgba(0,0,0,0.15);"
    ),
    tags$div(
      style = "font-size: 28px; font-weight: 700; line-height: 1.1;",
      tags$i(class = paste("fas", icon_class), style = "margin-right: 6px; font-size: 20px; opacity: 0.8;"),
      as.character(count)
    ),
    tags$div(
      style = "font-size: 12px; font-weight: 500; margin-top: 4px; text-transform: uppercase; letter-spacing: 0.5px; opacity: 0.9;",
      label
    )
  )
}

# ── UI ───────────────────────────────────────────────────────
live_status_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Custom CSS
    tags$style(HTML("
      .status-cards-row {
        display: flex;
        flex-wrap: wrap;
        gap: 0;
        margin-bottom: 16px;
      }
      .loading-overlay {
        position: relative;
        min-height: 200px;
      }
      .spinner-wrap {
        display: flex;
        justify-content: center;
        align-items: center;
        padding: 60px 0;
      }
      .spinner-wrap .spinner-border {
        width: 3rem;
        height: 3rem;
        border-width: 0.3em;
      }
    ")),
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
    div(class = "status-cards-row",
      uiOutput(ns("status_cards"))
    ),
    # Agent table
    bs4Card(
      title = "Agent Details",
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      uiOutput(ns("agent_table_wrap"))
    )
  )
}

# ── Server ───────────────────────────────────────────────────
live_status_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    auto_timer <- reactiveTimer(60000)
    is_loading <- reactiveVal(TRUE)

    # Reactive data
    live_data <- reactive({
      auto_timer()
      input$refresh_btn
      is_loading(TRUE)

      aws  <- tryCatch(read_latest_aws(), error = function(e) data.frame())
      five <- tryCatch(read_latest_five9(), error = function(e) data.frame())

      df <- bind_rows(aws, five)
      is_loading(FALSE)
      if (nrow(df) == 0) return(data.frame())

      df %>% mutate(duration_fmt = fmt_duration(status_duration))
    })

    # Update filter choices
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

    # Status cards
    output$status_cards <- renderUI({
      df <- filtered_data()

      if (is_loading()) {
        return(div(class = "spinner-wrap",
          div(class = "spinner-border text-primary", role = "status",
            span(class = "sr-only", "Loading...")
          )
        ))
      }

      if (nrow(df) == 0) {
        return(div(style = "padding: 20px; text-align: center; color: #999;",
          "No agents online"))
      }

      counts <- df %>%
        count(status_name, name = "n") %>%
        arrange(desc(n))

      # Build card list: Total first, then each status
      total_cfg <- get_config("Total")
      cards <- list(make_status_card("Total", nrow(df), total_cfg$bg, total_cfg$icon))

      for (i in seq_len(nrow(counts))) {
        cfg <- get_config(counts$status_name[i])
        cards[[length(cards) + 1]] <- make_status_card(
          counts$status_name[i], counts$n[i], cfg$bg, cfg$icon
        )
      }

      do.call(tagList, cards)
    })

    # Agent table with loading state
    output$agent_table_wrap <- renderUI({
      if (is_loading()) {
        return(div(class = "spinner-wrap",
          div(class = "spinner-border text-primary", role = "status",
            span(class = "sr-only", "Loading...")
          )
        ))
      }
      reactableOutput(ns("agent_table"))
    })

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
        defaultSorted = list(Status = "asc"),
        columns = list(
          `Agent Email` = colDef(minWidth = 220),
          Platform = colDef(width = 90, align = "center",
            cell = function(value) {
              bg <- if (value == "AWS") "#232f3e" else "#ff6a00"
              div(
                style = paste0(
                  "display: inline-block; padding: 2px 8px; border-radius: 4px; ",
                  "background: ", bg, "; color: white; font-size: 11px; font-weight: 600;"
                ),
                value
              )
            }
          ),
          Status = colDef(width = 130,
            cell = function(value) {
              cfg <- get_config(value)
              div(
                style = paste0(
                  "display: inline-block; padding: 3px 10px; border-radius: 4px; ",
                  "background: ", cfg$bg, "; color: white; font-weight: 600; font-size: 12px;"
                ),
                value
              )
            }
          ),
          Duration = colDef(width = 110, align = "right",
            style = list(fontFamily = "monospace", fontWeight = "bold")
          ),
          `Routing Profile` = colDef(minWidth = 150, na = "-")
        )
      )
    })

    # Last updated
    output$last_updated <- renderText({
      auto_timer()
      paste("Updated:", format(Sys.time(), "%H:%M:%S"))
    })
  })
}
