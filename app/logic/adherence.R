# ── Adherence logic ──────────────────────────────────────────

box::use(
  dplyr[...],
  lubridate[with_tz, hour, minute],
  app/logic/constants[INFRACTION_POINTS, PRODUCTIVE_STATES, APPROVED_STATES],
)

#' @export
detect_infractions <- function(spans, schedule) {
  if (nrow(spans) == 0 || nrow(schedule) == 0) return(data.frame())

  span_cst <- spans |>
    mutate(
      span_start_cst = with_tz(span_start_utc, "America/Chicago"),
      span_end_cst   = with_tz(span_end_utc, "America/Chicago"),
      date = as.Date(span_start_cst)
    )

  infractions <- list()

  for (i in seq_len(nrow(schedule))) {
    sched <- schedule[i, ]
    agent_spans <- span_cst |>
      filter(agent_email == sched$agent_email, date == sched$date)

    productive <- agent_spans |>
      filter(status_name %in% c(PRODUCTIVE_STATES, APPROVED_STATES))

    if (nrow(productive) == 0) {
      infractions[[length(infractions) + 1]] <- data.frame(
        agent_email = sched$agent_email,
        date = sched$date,
        infraction = "No Call / No Show",
        points = INFRACTION_POINTS$ncns,
        stringsAsFactors = FALSE
      )
      next
    }

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

#' @export
calculate_adherence <- function(intervals, schedule) {
  if (nrow(intervals) == 0 || nrow(schedule) == 0) return(data.frame())

  state_cols <- setdiff(names(intervals), c("agent_email", "platform", "date", "interval"))
  productive_cols <- intersect(state_cols, c(PRODUCTIVE_STATES, APPROVED_STATES))

  daily <- intervals |>
    group_by(agent_email, date) |>
    summarise(
      actual_secs = sum(across(any_of(productive_cols), sum)),
      .groups = "drop"
    ) |>
    mutate(actual_min = actual_secs / 60)

  result <- schedule |>
    left_join(daily, by = c("agent_email", "date")) |>
    mutate(
      actual_min = ifelse(is.na(actual_min), 0, actual_min),
      adherence_pct = ifelse(scheduled_min > 0,
        round(actual_min / scheduled_min * 100, 1), 0)
    )

  result
}

#' @export
calculate_points <- function(infractions) {
  if (nrow(infractions) == 0) return(data.frame())

  today <- Sys.Date()

  infractions |>
    group_by(agent_email) |>
    summarise(
      total_points = sum(points),
      infraction_count = n(),
      last_infraction = max(date),
      .groups = "drop"
    ) |>
    mutate(
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
}
