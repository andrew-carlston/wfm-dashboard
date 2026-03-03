# ── Tab 2: 15-Min Interval Grid (pre-computed) ────────────────

box::use(
  shiny[...],
  reactable[reactable, reactableOutput, renderReactable, colDef],
  dplyr[...],
  lubridate[with_tz],
  app/logic/supabase[read_agent_intervals],
)

#' @export
ui <- function(id) {
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
        div(class = "panel",
          div(class = "panel-header",
            paste("15-Minute Interval Grid \u2014", format(Sys.Date(), "%A, %B %d"))
          ),
          div(class = "panel-body", style = "overflow-x: auto;",
            reactableOutput(ns("grid_table"))
          )
        )
      )
    ),
    fluidRow(
      column(12,
        div(class = "panel panel-collapsed",
          div(class = "panel-header", "Summary: Agents per State per Interval"),
          div(class = "panel-body",
            reactableOutput(ns("summary_table"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, active_tab = reactive("live")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tab_visited <- reactiveVal(FALSE)
    observeEvent(active_tab(), {
      if (active_tab() == "grid") tab_visited(TRUE)
    })

    auto_timer <- reactiveTimer(60000)

    grid_data <- reactive({
      req(tab_visited())
      auto_timer()
      input$load_btn

      today <- as.Date(with_tz(Sys.time(), "America/Chicago"))
      message(paste("[GRID] Reading pre-computed intervals for:", today))

      intervals <- tryCatch(
        read_agent_intervals(today),
        error = function(e) {
          message(paste("[GRID] read_agent_intervals error:", e$message))
          data.frame()
        }
      )
      message(paste("[GRID] Pre-computed rows:", nrow(intervals)))

      if (nrow(intervals) == 0) return(list(grid = data.frame(), summary = data.frame()))

      # Apply platform filter
      if (input$platform_filter != "All") {
        intervals <- intervals |> filter(platform == input$platform_filter)
      }

      state_cols <- setdiff(names(intervals), c("agent_email", "platform", "date", "interval"))
      summary_df <- intervals |>
        group_by(interval) |>
        summarise(across(all_of(state_cols), ~ sum(. > 0)), .groups = "drop") |>
        arrange(interval)

      list(grid = intervals, summary = summary_df)
    })

    observeEvent(grid_data(), {
      df <- grid_data()$grid
      if (nrow(df) == 0) return()
      agents <- sort(unique(df$agent_email))
      updateSelectInput(session, "agent_filter",
        choices = c("All", agents), selected = input$agent_filter)
    })

    output$grid_table <- renderReactable({
      gd <- grid_data()
      df <- gd$grid
      if (nrow(df) == 0) {
        return(reactable(data.frame(Message = "No interval data for today."),
          columns = list(Message = colDef(align = "center"))))
      }

      if (!is.null(input$agent_filter) && input$agent_filter != "All") {
        df <- df |> filter(agent_email == input$agent_filter)
      }

      state_cols <- setdiff(names(df), c("agent_email", "platform", "date", "interval"))

      display_df <- df |>
        mutate(across(all_of(state_cols), ~ round(. / 60, 1))) |>
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

    output$last_updated <- renderText({
      auto_timer()
      paste("Updated:", format(Sys.time(), "%H:%M:%S"))
    })
  })
}
