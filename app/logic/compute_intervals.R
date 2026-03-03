# ── Background job: compute interval grid → write to Supabase ─

box::use(
  dplyr[bind_rows, mutate, group_by, summarise, select],
  tidyr[pivot_longer],
  lubridate[with_tz],
  later[later],
  app/logic/supabase[read_aws_snapshots, read_five9_snapshots, write_agent_intervals],
  app/logic/grid[build_spans, spans_to_intervals],
)

COMPUTE_INTERVAL_SECS <- 300  # 5 minutes

run_interval_computation <- function() {
  message("[COMPUTE] Starting interval computation...")

  aws <- tryCatch(read_aws_snapshots(36), error = function(e) {
    message(paste("[COMPUTE] AWS error:", e$message))
    data.frame()
  })
  five <- tryCatch(read_five9_snapshots(36), error = function(e) {
    message(paste("[COMPUTE] Five9 error:", e$message))
    data.frame()
  })

  all_snaps <- bind_rows(aws, five)
  message(paste("[COMPUTE] Total snapshots:", nrow(all_snaps)))

  if (nrow(all_snaps) == 0) {
    message("[COMPUTE] No snapshots, skipping")
    return(invisible(NULL))
  }

  spans <- tryCatch(build_spans(all_snaps), error = function(e) {
    message(paste("[COMPUTE] build_spans error:", e$message))
    data.frame()
  })
  message(paste("[COMPUTE] Spans:", nrow(spans)))

  if (nrow(spans) == 0) {
    message("[COMPUTE] No spans, skipping")
    return(invisible(NULL))
  }

  intervals <- tryCatch(spans_to_intervals(spans), error = function(e) {
    message(paste("[COMPUTE] spans_to_intervals error:", e$message))
    data.frame()
  })
  message(paste("[COMPUTE] Intervals:", nrow(intervals)))

  if (nrow(intervals) == 0) {
    message("[COMPUTE] No intervals, skipping")
    return(invisible(NULL))
  }

  # Pivot to long format for upsert
  state_cols <- setdiff(names(intervals), c("agent_email", "platform", "date", "interval"))

  long_df <- intervals |>
    pivot_longer(
      cols = all_of(state_cols),
      names_to = "status_name",
      values_to = "seconds"
    ) |>
    mutate(
      date = as.character(date),
      seconds = as.integer(seconds),
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S+00:00")
    ) |>
    select(date, interval, agent_email, platform, status_name, seconds, updated_at)

  # Filter out zero-second rows to reduce upsert volume
  long_df <- long_df[long_df$seconds > 0, ]
  message(paste("[COMPUTE] Non-zero rows to upsert:", nrow(long_df)))

  tryCatch(
    write_agent_intervals(long_df),
    error = function(e) message(paste("[COMPUTE] Write error:", e$message))
  )

  message("[COMPUTE] Done")
  invisible(NULL)
}

#' @export
start_interval_job <- function() {
  message("[COMPUTE] Starting background interval job (every 5 min)")

  # Run once immediately
  tryCatch(
    run_interval_computation(),
    error = function(e) message(paste("[COMPUTE] Initial run error:", e$message))
  )

  # Schedule recurring
  schedule_next <- function() {
    later(function() {
      tryCatch(
        run_interval_computation(),
        error = function(e) message(paste("[COMPUTE] Scheduled run error:", e$message))
      )
      schedule_next()
    }, delay = COMPUTE_INTERVAL_SECS)
  }

  schedule_next()
  message("[COMPUTE] Background job scheduled")
}
