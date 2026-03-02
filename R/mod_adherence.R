# ── Tab 3: Adherence Report ───────────────────────────────────
# Schedule vs actual adherence with attendance policy point system.

library(shiny)
library(bs4Dash)
library(reactable)
library(dplyr)
library(tidyr)
library(lubridate)

source("R/data_supabase.R")
source("R/data_grid.R")
source("R/data_schedule.R")

# ── Attendance policy point rules (v2.2) ─────────────────────
INFRACTION_POINTS <- list(
  tardy_short  = 1,   # >5 min, <60 min late
  tardy_long   = 2,   # >60 min late
  left_early_under50 = 3,  # <50% shift worked
  left_early_over50  = 2,  # >50% shift worked
  ncns         = 5,   # no call / no show
  missed_meeting = 1  # missed meeting / training
)

POINT_THRESHOLDS <- data.frame(
  points = c(6, 8, 10, 12),
  action = c("Verbal Warning", "Written Warning", "Final Warning", "Termination"),
  floor  = c(4, 6, 8, NA),
  stringsAsFactors = FALSE
)

# ── Productive states (count toward adherence) ───────────────
PRODUCTIVE_STATES <- c("Available", "Ready", "On Call", "On call", "ACW",
                        "After Call Work", "Chat", "Outbound")
APPROVED_STATES <- c("Break", "Lunch", "Meeting", "Training", "Coaching")

# ── Detect infractions from snapshots vs schedule ────────────
detect_infractions <- function(spans, schedule) {
  if (nrow(spans) == 0 || nrow(schedule) == 0) return(data.frame())

  # Convert spans to CST for comparison with schedule
  span_cst <- spans %>%
    mutate(
      span_start_cst = with_tz(span_start_utc, "America/Chicago"),
      span_end_cst   = with_tz(span_end_utc, "America/Chicago"),
      date = as.Date(span_start_cst)
    )

  # Join spans with schedule
  infractions <- list()

  for (i in seq_len(nrow(schedule))) {
    sched <- schedule[i, ]
    agent_spans <- span_cst %>%
      filter(agent_email == sched$agent_email, date == sched$date)

    # Get productive spans
    productive <- agent_spans %>%
      filter(status_name %in% c(PRODUCTIVE_STATES, APPROVED_STATES))

    if (nrow(productive) == 0) {
      # No call / no show
      infractions[[length(infractions) + 1]] <- data.frame(
        agent_email = sched$agent_email,
        date = sched$date,
        infraction = "No Call / No Show",
        points = INFRACTION_POINTS$ncns,
        stringsAsFactors = FALSE
      )
      next
    }

    # Check tardy: first productive state vs shift_start
    first_productive_min <- min(
      hour(productive$span_start_cst) * 60 + minute(productive$span_start_cst)
    )
    late_min <- first_productive_min - sched$shift_start_min

    if (late_min > 5 && late_min <= 60) {
      infractions[[length(infractions) + 1]] <- data.frame(
        agent_email = sched$agent_email,
        date = sched$date,
        infraction = paste0("Tardy (", round(late_min), " min)"),
        points = INFRACTION_POINTS$tardy_short,
        stringsAsFactors = FALSE
      )
    } else if (late_min > 60) {
      infractions[[length(infractions) + 1]] <- data.frame(
        agent_email = sched$agent_email,
        date = sched$date,
        infraction = paste0("Tardy (", round(late_min), " min)"),
        points = INFRACTION_POINTS$tardy_long,
        stringsAsFactors = FALSE
      )
    }

    # Check left early: last productive state vs shift_end
    last_productive_min <- max(
      hour(productive$span_end_cst) * 60 + minute(productive$span_end_cst)
    )
    early_min <- sched$shift_end_min - last_productive_min

    if (early_min > 15) {
      worked_min <- last_productive_min - first_productive_min
      pct_worked <- worked_min / sched$scheduled_min

      if (pct_worked < 0.5) {
        infractions[[length(infractions) + 1]] <- data.frame(
          agent_email = sched$agent_email,
          date = sched$date,
          infraction = paste0("Left Early (<50%, ", round(early_min), " min)"),
          points = INFRACTION_POINTS$left_early_under50,
          stringsAsFactors = FALSE
        )
      } else {
        infractions[[length(infractions) + 1]] <- data.frame(
          agent_email = sched$agent_email,
          date = sched$date,
          infraction = paste0("Left Early (>50%, ", round(early_min), " min)"),
          points = INFRACTION_POINTS$left_early_over50,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(infractions) == 0) return(data.frame())
  bind_rows(infractions)
}

# ── Calculate adherence per agent per day ────────────────────
calculate_adherence <- function(intervals, schedule) {
  if (nrow(intervals) == 0 || nrow(schedule) == 0) return(data.frame())

  # Productive + approved seconds per agent per date
  state_cols <- setdiff(names(intervals), c("agent_email", "platform", "date", "interval"))

  productive_cols <- intersect(state_cols, c(PRODUCTIVE_STATES, APPROVED_STATES))

  daily <- intervals %>%
    group_by(agent_email, date) %>%
    summarise(
      actual_secs = sum(across(any_of(productive_cols), sum)),
      .groups = "drop"
    ) %>%
    mutate(actual_min = actual_secs / 60)

  # Join with schedule
  result <- schedule %>%
    left_join(daily, by = c("agent_email", "date")) %>%
    mutate(
      actual_min = ifelse(is.na(actual_min), 0, actual_min),
      adherence_pct = ifelse(scheduled_min > 0,
        round(actual_min / scheduled_min * 100, 1), 0)
    )

  result
}

# ── Calculate point balances with roll-off ───────────────────
calculate_points <- function(infractions) {
  if (nrow(infractions) == 0) return(data.frame())

  # Points by agent, with monthly roll-off
  today <- Sys.Date()

  agent_points <- infractions %>%
    group_by(agent_email) %>%
    summarise(
      total_points = sum(points),
      infraction_count = n(),
      last_infraction = max(date),
      .groups = "drop"
    ) %>%
    mutate(
      # Roll-off: 1 point per clean calendar month since last infraction
      months_clean = as.integer(difftime(today, last_infraction, units = "days")) %/% 30,
      adjusted_points = pmax(0, total_points - months_clean),
      warning_level = case_when(
        adjusted_points >= 12 ~ "Termination",
        adjusted_points >= 10 ~ "Final Warning",
        adjusted_points >= 8  ~ "Written Warning",
        adjusted_points >= 6  ~ "Verbal Warning",
        TRUE                  ~ "Good Standing"
      )
    )

  agent_points
}

# ── UI ───────────────────────────────────────────────────────
adherence_ui <- function(id) {
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
        bs4Card(
          title = "Agent Adherence Summary",
          status = "primary",
          width = 12,
          solidHeader = TRUE,
          reactableOutput(ns("adherence_table"))
        )
      )
    ),
    fluidRow(
      column(6,
        bs4Card(
          title = "Attendance Points",
          status = "warning",
          width = 12,
          solidHeader = TRUE,
          reactableOutput(ns("points_table"))
        )
      ),
      column(6,
        bs4Card(
          title = "Infraction Log",
          status = "danger",
          width = 12,
          solidHeader = TRUE,
          reactableOutput(ns("infractions_table"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Card(
          title = "Agent Detail (click an agent above)",
          status = "info",
          width = 12,
          solidHeader = TRUE,
          collapsed = TRUE,
          reactableOutput(ns("detail_table"))
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
adherence_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Schedule data (cached)
    schedule_data <- reactiveVal(data.frame())

    observeEvent(input$refresh_schedule, {
      sched <- tryCatch(read_schedule(), error = function(e) {
        showNotification(paste("Schedule error:", e$message), type = "error")
        data.frame()
      })
      schedule_data(sched)
    }, ignoreNULL = FALSE)

    # Load on first visit
    observe({
      if (nrow(schedule_data()) == 0) {
        sched <- tryCatch(read_schedule(), error = function(e) data.frame())
        schedule_data(sched)
      }
    }) %>% bindEvent(TRUE, once = TRUE)

    # Selected agent for drill-down
    selected_agent <- reactiveVal(NULL)

    # Main report data
    report_data <- eventReactive(input$load_btn, {
      dates <- input$date_range
      hours_back <- as.numeric(difftime(Sys.time(),
        as.POSIXct(paste(dates[1], "00:00:00"), tz = "America/Chicago"),
        units = "hours")) + 24

      # Read snapshots
      aws  <- tryCatch(read_aws_snapshots(hours_back), error = function(e) data.frame())
      five <- tryCatch(read_five9_snapshots(hours_back), error = function(e) data.frame())
      all_snaps <- bind_rows(aws, five)

      if (input$platform_filter != "All") {
        all_snaps <- all_snaps %>% filter(platform == input$platform_filter)
      }

      if (nrow(all_snaps) == 0) {
        return(list(
          adherence = data.frame(),
          infractions = data.frame(),
          points = data.frame(),
          intervals = data.frame()
        ))
      }

      # Build spans and intervals
      spans <- build_spans(all_snaps)
      intervals <- if (nrow(spans) > 0) spans_to_intervals(spans) else data.frame()

      # Filter schedule to date range
      sched <- schedule_data()
      if (nrow(sched) > 0) {
        sched <- sched %>% filter(date >= dates[1], date <= dates[2])
      }

      # Calculate adherence
      adherence <- if (nrow(intervals) > 0 && nrow(sched) > 0) {
        calculate_adherence(intervals, sched)
      } else {
        data.frame()
      }

      # Detect infractions
      infractions <- if (nrow(spans) > 0 && nrow(sched) > 0) {
        detect_infractions(spans, sched)
      } else {
        data.frame()
      }

      # Calculate points
      points <- if (nrow(infractions) > 0) calculate_points(infractions) else data.frame()

      list(
        adherence = adherence,
        infractions = infractions,
        points = points,
        intervals = intervals
      )
    }, ignoreNULL = FALSE)

    # Adherence summary table
    output$adherence_table <- renderReactable({
      adh <- report_data()$adherence
      if (nrow(adh) == 0) {
        return(reactable(data.frame(Message = "No adherence data. Load schedule + snapshots."),
          columns = list(Message = colDef(align = "center"))))
      }

      summary_df <- adh %>%
        group_by(agent_email) %>%
        summarise(
          days = n(),
          scheduled_hrs = round(sum(scheduled_min) / 60, 1),
          actual_hrs = round(sum(actual_min) / 60, 1),
          adherence_pct = round(sum(actual_min) / sum(scheduled_min) * 100, 1),
          .groups = "drop"
        ) %>%
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

    # Handle agent selection for drill-down
    observeEvent(input$adherence_table__selected, {
      adh <- report_data()$adherence
      if (nrow(adh) == 0) return()

      agents <- adh %>%
        group_by(agent_email) %>%
        summarise(.groups = "drop") %>%
        arrange(agent_email)

      idx <- input$adherence_table__selected
      if (length(idx) > 0 && idx <= nrow(agents)) {
        selected_agent(agents$agent_email[idx])
      }
    })

    # Points table
    output$points_table <- renderReactable({
      pts <- report_data()$points
      if (nrow(pts) == 0) {
        return(reactable(data.frame(Message = "No points data."),
          columns = list(Message = colDef(align = "center"))))
      }

      reactable(
        pts %>% select(
          Agent = agent_email,
          `Total Points` = total_points,
          `Months Clean` = months_clean,
          `Adjusted Points` = adjusted_points,
          `Warning Level` = warning_level
        ),
        columns = list(
          `Warning Level` = colDef(
            cell = function(value) {
              color <- case_when(
                value == "Termination"     ~ "#dc3545",
                value == "Final Warning"   ~ "#fd7e14",
                value == "Written Warning" ~ "#ffc107",
                value == "Verbal Warning"  ~ "#17a2b8",
                TRUE                       ~ "#28a745"
              )
              div(style = paste0("color: ", color, "; font-weight: bold;"), value)
            }
          )
        ),
        striped = TRUE,
        highlight = TRUE
      )
    })

    # Infractions log
    output$infractions_table <- renderReactable({
      inf <- report_data()$infractions
      if (nrow(inf) == 0) {
        return(reactable(data.frame(Message = "No infractions detected."),
          columns = list(Message = colDef(align = "center"))))
      }

      reactable(
        inf %>%
          arrange(desc(date)) %>%
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

    # Agent detail drill-down
    output$detail_table <- renderReactable({
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

      agent_data <- intervals %>% filter(agent_email == agent)
      if (nrow(agent_data) == 0) {
        return(reactable(data.frame(Message = paste("No data for", agent)),
          columns = list(Message = colDef(align = "center"))))
      }

      state_cols <- setdiff(names(agent_data), c("agent_email", "platform", "date", "interval"))
      display_df <- agent_data %>%
        mutate(across(all_of(state_cols), ~ round(. / 60, 1))) %>%
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
