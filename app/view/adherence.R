# ── Tab 3: Adherence Report ───────────────────────────────────

box::use(
  shiny[...],
  reactable[reactable, reactableOutput, renderReactable, colDef],
  dplyr[...],
  lubridate[...],
  app/logic/supabase[read_aws_snapshots, read_five9_snapshots],
  app/logic/grid[build_spans, spans_to_intervals],
  app/logic/schedule[read_schedule],
  app/logic/adherence[detect_infractions, calculate_adherence, calculate_points],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, dateRangeInput(ns("date_range"), "Date Range",
        start = Sys.Date() - 7, end = Sys.Date())),
      column(3, selectInput(ns("platform_filter"), "Platform",
        choices = c("All", "AWS", "Five9"), selected = "All")),
      column(3, div(style = "padding-top: 25px;",
        actionButton(ns("load_btn"), "Load Report", icon = icon("chart-bar"),
                     class = "btn-primary btn-sm")
      )),
      column(3, div(style = "padding-top: 25px;",
        actionButton(ns("refresh_schedule"), "Refresh Schedule", icon = icon("calendar"),
                     class = "btn-outline-secondary btn-sm")
      ))
    ),
    fluidRow(
      column(12,
        div(class = "panel",
          div(class = "panel-header", "Agent Adherence Summary"),
          div(class = "panel-body",
            reactableOutput(ns("adherence_table"))
          )
        )
      )
    ),
    fluidRow(
      column(6,
        div(class = "panel",
          div(class = "panel-header panel-warning", "Attendance Points"),
          div(class = "panel-body",
            reactableOutput(ns("points_table"))
          )
        )
      ),
      column(6,
        div(class = "panel",
          div(class = "panel-header panel-danger", "Infraction Log"),
          div(class = "panel-body",
            reactableOutput(ns("infractions_table"))
          )
        )
      )
    ),
    fluidRow(
      column(12,
        div(class = "panel panel-collapsed",
          div(class = "panel-header panel-info", "Agent Detail (click an agent above)"),
          div(class = "panel-body",
            reactableOutput(ns("detail_table"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    schedule_data <- reactiveVal(data.frame())

    observeEvent(input$refresh_schedule, {
      sched <- tryCatch(read_schedule(), error = function(e) {
        showNotification(paste("Schedule error:", e$message), type = "error")
        data.frame()
      })
      schedule_data(sched)
    })

    selected_agent <- reactiveVal(NULL)

    report_data <- eventReactive(input$load_btn, {
      dates <- input$date_range
      hours_back <- as.numeric(difftime(Sys.time(),
        as.POSIXct(paste(dates[1], "00:00:00"), tz = "America/Chicago"),
        units = "hours")) + 24

      aws  <- tryCatch(read_aws_snapshots(hours_back), error = function(e) data.frame())
      five <- tryCatch(read_five9_snapshots(hours_back), error = function(e) data.frame())
      all_snaps <- bind_rows(aws, five)

      if (input$platform_filter != "All") {
        all_snaps <- all_snaps |> filter(platform == input$platform_filter)
      }

      if (nrow(all_snaps) == 0) {
        return(list(
          adherence = data.frame(),
          infractions = data.frame(),
          points = data.frame(),
          intervals = data.frame()
        ))
      }

      spans <- build_spans(all_snaps)
      intervals <- if (nrow(spans) > 0) spans_to_intervals(spans) else data.frame()

      sched <- schedule_data()
      if (nrow(sched) > 0) {
        sched <- sched |> filter(date >= dates[1], date <= dates[2])
      }

      adherence <- if (nrow(intervals) > 0 && nrow(sched) > 0) {
        calculate_adherence(intervals, sched)
      } else {
        data.frame()
      }

      infractions <- if (nrow(spans) > 0 && nrow(sched) > 0) {
        detect_infractions(spans, sched)
      } else {
        data.frame()
      }

      points <- if (nrow(infractions) > 0) calculate_points(infractions) else data.frame()

      list(
        adherence = adherence,
        infractions = infractions,
        points = points,
        intervals = intervals
      )
    })

    output$adherence_table <- renderReactable({
      req(input$load_btn)
      adh <- report_data()$adherence
      if (nrow(adh) == 0) {
        return(reactable(data.frame(Message = "No adherence data. Load schedule + snapshots."),
          columns = list(Message = colDef(align = "center"))))
      }

      summary_df <- adh |>
        group_by(agent_email) |>
        summarise(
          days = n(),
          scheduled_hrs = round(sum(scheduled_min) / 60, 1),
          actual_hrs = round(sum(actual_min) / 60, 1),
          adherence_pct = round(sum(actual_min) / sum(scheduled_min) * 100, 1),
          .groups = "drop"
        ) |>
        arrange(adherence_pct)

      reactable(
        summary_df,
        columns = list(
          agent_email = colDef(name = "Agent"),
          days = colDef(name = "Days"),
          scheduled_hrs = colDef(name = "Scheduled Hrs", align = "right"),
          actual_hrs = colDef(name = "Actual Hrs", align = "right"),
          adherence_pct = colDef(
            name = "Adherence %",
            align = "right",
            cell = function(value) {
              color <- if (value >= 95) "#28a745" else if (value >= 85) "#ffc107" else "#dc3545"
              div(style = paste0("color: ", color, "; font-weight: bold;"),
                  paste0(value, "%"))
            }
          )
        ),
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        onClick = "select",
        selection = "single"
      )
    })

    observeEvent(input$adherence_table__selected, {
      adh <- report_data()$adherence
      if (nrow(adh) == 0) return()

      agents <- adh |>
        group_by(agent_email) |>
        summarise(.groups = "drop") |>
        arrange(agent_email)

      idx <- input$adherence_table__selected
      if (length(idx) > 0 && idx <= nrow(agents)) {
        selected_agent(agents$agent_email[idx])
      }
    })

    output$points_table <- renderReactable({
      req(input$load_btn)
      pts <- report_data()$points
      if (nrow(pts) == 0) {
        return(reactable(data.frame(Message = "No points data."),
          columns = list(Message = colDef(align = "center"))))
      }

      reactable(
        pts |> select(
          Agent = agent_email,
          `Total Points` = total_points,
          `Months Clean` = months_clean,
          `Adjusted Points` = adjusted_points,
          `Warning Level` = warning_level
        ),
        columns = list(
          `Warning Level` = colDef(
            cell = function(value) {
              color <- if (value == "Termination") "#dc3545"
                else if (value == "Final Warning") "#fd7e14"
                else if (value == "Written Warning") "#ffc107"
                else if (value == "Verbal Warning") "#17a2b8"
                else "#28a745"
              div(style = paste0("color: ", color, "; font-weight: bold;"), value)
            }
          )
        ),
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$infractions_table <- renderReactable({
      req(input$load_btn)
      inf <- report_data()$infractions
      if (nrow(inf) == 0) {
        return(reactable(data.frame(Message = "No infractions detected."),
          columns = list(Message = colDef(align = "center"))))
      }

      reactable(
        inf |>
          arrange(desc(date)) |>
          select(
            Agent = agent_email,
            Date = date,
            Infraction = infraction,
            Points = points
          ),
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$detail_table <- renderReactable({
      req(input$load_btn)
      agent <- selected_agent()
      if (is.null(agent)) {
        return(reactable(data.frame(Message = "Select an agent from the summary table."),
          columns = list(Message = colDef(align = "center"))))
      }

      intervals <- report_data()$intervals
      if (nrow(intervals) == 0) {
        return(reactable(data.frame(Message = "No interval data."),
          columns = list(Message = colDef(align = "center"))))
      }

      agent_data <- intervals |> filter(agent_email == agent)
      if (nrow(agent_data) == 0) {
        return(reactable(data.frame(Message = paste("No data for", agent)),
          columns = list(Message = colDef(align = "center"))))
      }

      state_cols <- setdiff(names(agent_data), c("agent_email", "platform", "date", "interval"))
      display_df <- agent_data |>
        mutate(across(all_of(state_cols), ~ round(. / 60, 1))) |>
        select(Date = date, Interval = interval, all_of(state_cols))

      reactable(
        display_df,
        striped = TRUE,
        compact = TRUE,
        defaultPageSize = 96
      )
    })
  })
}
