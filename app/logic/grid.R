# ── Shared grid functions ─────────────────────────────────────
# 15-min interval grid: make_grid, split_midnight, allocate_to_grid, build_spans, spans_to_intervals

box::use(
  dplyr[...],
  tidyr[crossing, pivot_wider],
  lubridate[with_tz, hour, minute, second, floor_date, ymd_hms],
)

#' @export
make_grid <- function() {
  data.frame(slot_min = seq(0, 23 * 60 + 45, by = 15)) |>
    mutate(
      slot_end_min = slot_min + 15,
      interval = sprintf("%02d:%02d", slot_min %/% 60, slot_min %% 60)
    )
}

#' @export
split_midnight <- function(df, date_col = "date") {
  same_day <- df |> filter(end_min <= 1440)
  overflow <- df |> filter(end_min > 1440)
  if (nrow(overflow) == 0) return(same_day)
  overflow_today <- overflow |> mutate(end_min = 1440)
  overflow_next  <- overflow |>
    mutate(!!date_col := !!sym(date_col) + 1, end_min = end_min - 1440, start_min = 0)
  bind_rows(same_day, overflow_today, overflow_next)
}

#' @export
allocate_to_grid <- function(df) {
  grid <- make_grid()
  message(paste("[GRID] allocate_to_grid: input rows =", nrow(df), "x 96 grid slots"))
  result <- df |>
    crossing(grid) |>
    filter(start_min < slot_end_min, end_min > slot_min) |>
    mutate(
      clip_start = pmax(start_min, slot_min),
      clip_end   = pmin(end_min, slot_end_min),
      secs       = round((clip_end - clip_start) * 60)
    ) |>
    filter(secs > 0)
  message(paste("[GRID] allocate_to_grid: output rows =", nrow(result)))
  result
}

#' @export
build_spans <- function(snapshots) {
  if (nrow(snapshots) == 0) return(data.frame())

  df <- snapshots |>
    filter(!is.na(status_start_utc)) |>
    arrange(agent_email, snapshot_ts)

  spans <- df |>
    group_by(agent_email) |>
    mutate(
      prev_status = lag(status_name),
      prev_start  = lag(status_start_utc),
      is_new_span = is.na(prev_status) |
                    status_name != prev_status |
                    status_start_utc != prev_start,
      span_id = cumsum(is_new_span)
    ) |>
    group_by(agent_email, span_id) |>
    summarise(
      platform       = first(platform),
      status_name    = first(status_name),
      span_start_utc = first(status_start_utc),
      last_seen_ts   = max(snapshot_ts),
      .groups = "drop"
    ) |>
    arrange(agent_email, span_start_utc)

  spans <- spans |>
    group_by(agent_email) |>
    mutate(
      span_end_utc = lead(span_start_utc, default = with_tz(Sys.time(), "UTC"))
    ) |>
    ungroup()

  spans
}

#' @export
spans_to_intervals <- function(spans) {
  if (nrow(spans) == 0) return(data.frame())

  df <- spans |>
    mutate(
      span_start_cst = with_tz(span_start_utc, "America/Chicago"),
      span_end_cst   = with_tz(span_end_utc, "America/Chicago"),
      date      = as.Date(span_start_cst),
      start_min = hour(span_start_cst) * 60 + minute(span_start_cst) +
                  second(span_start_cst) / 60,
      end_min   = as.numeric(difftime(span_end_cst, floor_date(span_start_cst, "day"),
                                       units = "mins"))
    ) |>
    filter(end_min > start_min)

  df <- split_midnight(df)
  allocated <- allocate_to_grid(df)

  result <- allocated |>
    group_by(agent_email, platform, date, interval, status_name) |>
    summarise(secs = sum(secs), .groups = "drop") |>
    pivot_wider(
      names_from  = status_name,
      values_from = secs,
      values_fill = 0
    ) |>
    arrange(agent_email, date, interval)

  result
}
