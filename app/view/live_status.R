# ── Tab 1: Live Agent Status ──────────────────────────────────

box::use(
  shiny[...],
  dplyr[...],
  lubridate[...],
  app/logic/supabase[read_latest_aws, read_latest_five9],
  app/logic/constants[STATUS_CONFIG, get_config, fmt_duration],
)

#' @export
ui <- function(id) {
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
    uiOutput(ns("summary_bar")),
    uiOutput(ns("status_cards"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    auto_timer <- reactiveTimer(60000)

    live_data <- reactive({
      auto_timer()
      input$refresh_btn

      message("[LIVE] Fetching latest snapshots...")
      aws  <- tryCatch({
        res <- read_latest_aws()
        message(paste("[LIVE] AWS rows:", nrow(res)))
        res
      }, error = function(e) {
        message(paste("[LIVE] AWS error:", e$message))
        data.frame()
      })
      five <- tryCatch({
        res <- read_latest_five9()
        message(paste("[LIVE] Five9 rows:", nrow(res)))
        res
      }, error = function(e) {
        message(paste("[LIVE] Five9 error:", e$message))
        data.frame()
      })

      df <- bind_rows(aws, five)
      message(paste("[LIVE] Total rows:", nrow(df)))
      if (nrow(df) == 0) return(data.frame())

      df <- df |>
        filter(!(status_name == "Offline" & status_duration > 86400))

      if (nrow(df) == 0) return(data.frame())

      df |> mutate(duration_fmt = fmt_duration(status_duration))
    })

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

    filtered_data <- reactive({
      df <- live_data()
      if (nrow(df) == 0) return(data.frame())

      if (input$platform_filter != "All") {
        df <- df |> filter(platform == input$platform_filter)
      }
      if (!is.null(input$status_filter) && input$status_filter != "All") {
        df <- df |> filter(status_name == input$status_filter)
      }
      if (!is.null(input$routing_filter) && input$routing_filter != "All") {
        df <- df |> filter(routing_profile == input$routing_filter)
      }
      df
    })

    output$summary_bar <- renderUI({
      df <- filtered_data()
      if (nrow(df) == 0) return(NULL)

      counts <- df |>
        count(status_name, name = "n") |>
        mutate(order = sapply(status_name, function(s) get_config(s)$order)) |>
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

    output$status_cards <- renderUI({
      df <- filtered_data()
      if (nrow(df) == 0) {
        return(tags$div(
          style = "text-align:center; padding:60px 0; color:#999;",
          tags$i(class = "fas fa-users", style = "font-size: 40px;"),
          tags$div(style = "margin-top:12px;", "No agents online")
        ))
      }

      df <- df |>
        mutate(status_order = sapply(status_name, function(s) get_config(s)$order)) |>
        arrange(status_order, agent_email)

      statuses <- df |>
        group_by(status_name) |>
        summarise(n = n(), order = first(status_order), .groups = "drop") |>
        arrange(order)

      cards <- lapply(seq_len(nrow(statuses)), function(i) {
        status <- statuses$status_name[i]
        count <- statuses$n[i]
        cfg <- get_config(status)

        agents <- df |>
          filter(status_name == status) |>
          arrange(desc(status_duration))

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

    output$last_updated <- renderText({
      auto_timer()
      paste("Updated:", format(Sys.time(), "%H:%M:%S"))
    })
  })
}
