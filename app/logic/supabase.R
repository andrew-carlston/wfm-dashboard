# ── Supabase snapshot readers + interval writers ─────────────

box::use(
  httr[GET, POST, add_headers, content, status_code],
  jsonlite[fromJSON, toJSON],
  dplyr[...],
  tidyr[pivot_wider],
  lubridate[ymd_hms, hours, with_tz],
)

SUPABASE_URL <- Sys.getenv("SUPABASE_URL", "https://cfiozaadhfmgliffrdvh.supabase.co")
SUPABASE_KEY <- Sys.getenv("SUPABASE_KEY", "")

# ── Generic paginated Supabase reader ────────────────────────
#' @export
read_supabase_table <- function(table, select, filter_col, cutoff, order_by) {
  if (SUPABASE_KEY == "") stop("Set SUPABASE_KEY env var")

  all_rows <- list()
  offset <- 0
  page_size <- 1000

  repeat {
    query <- list(
      select = select,
      offset = offset,
      limit = page_size
    )
    query[[filter_col]] <- paste0("gte.", cutoff)
    query[["order"]] <- order_by

    resp <- GET(
      paste0(SUPABASE_URL, "/rest/v1/", table),
      query = query,
      add_headers(
        apikey = SUPABASE_KEY,
        Authorization = paste("Bearer", SUPABASE_KEY)
      )
    )

    if (status_code(resp) != 200) {
      stop(paste("Supabase error:", status_code(resp), content(resp, as = "text")))
    }

    page <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    if (is.null(page) || nrow(page) == 0) break

    all_rows[[length(all_rows) + 1]] <- page
    if (nrow(page) < page_size) break
    offset <- offset + page_size
  }

  if (length(all_rows) == 0) return(data.frame())
  bind_rows(all_rows)
}

# ── AWS Connect snapshots ────────────────────────────────────
#' @export
read_aws_snapshots <- function(hours_back = 48) {
  cutoff <- format(Sys.time() - hours(hours_back), "%Y-%m-%dT%H:%M:%S+00:00")

  df <- read_supabase_table(
    table = "aws_agent_snapshots",
    select = "snapshot_ts,user_id,agent_email,status_name,status_start_utc,status_duration,routing_profile,contacts",
    filter_col = "snapshot_ts",
    cutoff = cutoff,
    order_by = "agent_email.asc,snapshot_ts.asc"
  )

  if (nrow(df) == 0) return(data.frame())

  df |>
    mutate(
      platform = "AWS",
      snapshot_ts = ymd_hms(snapshot_ts, tz = "UTC"),
      status_start_utc = ymd_hms(status_start_utc, tz = "UTC")
    )
}

# ── Five9 snapshots ──────────────────────────────────────────
#' @export
read_five9_snapshots <- function(hours_back = 48) {
  cutoff <- format(Sys.time() - hours(hours_back), "%Y-%m-%dT%H:%M:%S+00:00")

  df <- read_supabase_table(
    table = "five9_agent_snapshots",
    select = "snapshot_ts,username,full_name,state,reason_code,state_since_utc,state_duration",
    filter_col = "snapshot_ts",
    cutoff = cutoff,
    order_by = "username.asc,snapshot_ts.asc"
  )

  if (nrow(df) == 0) return(data.frame())

  df |>
    mutate(
      platform = "Five9",
      agent_email = username,
      status_name = case_when(
        state == "Ready"           ~ "Available",
        state == "Not Ready" & reason_code != "" & !is.na(reason_code) ~ reason_code,
        state == "Not Ready"       ~ "Not Ready",
        state == "On Call"         ~ "On Call",
        state == "After Call Work" ~ "ACW",
        TRUE                       ~ state
      ),
      snapshot_ts = ymd_hms(snapshot_ts, tz = "UTC"),
      status_start_utc = ymd_hms(state_since_utc, tz = "UTC"),
      status_duration = as.integer(state_duration) %/% 1000L,
      routing_profile = NA_character_,
      contacts = NA_character_
    ) |>
    select(platform, snapshot_ts, agent_email, status_name,
           status_start_utc, status_duration, routing_profile, contacts)
}

# ── Latest snapshot only (for live status tab) ───────────────
read_latest_snapshot <- function(table, select, order_by) {
  if (SUPABASE_KEY == "") stop("Set SUPABASE_KEY env var")

  resp <- GET(
    paste0(SUPABASE_URL, "/rest/v1/", table),
    query = list(
      select = "snapshot_ts",
      order = "snapshot_ts.desc",
      limit = 1
    ),
    add_headers(
      apikey = SUPABASE_KEY,
      Authorization = paste("Bearer", SUPABASE_KEY)
    )
  )

  if (status_code(resp) != 200) return(data.frame())
  ts_row <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  if (is.null(ts_row) || nrow(ts_row) == 0) return(data.frame())

  latest_ts <- ts_row$snapshot_ts[1]

  read_supabase_table(
    table = table,
    select = select,
    filter_col = "snapshot_ts",
    cutoff = latest_ts,
    order_by = order_by
  )
}

#' @export
read_latest_aws <- function() {
  df <- read_latest_snapshot(
    table = "aws_agent_snapshots",
    select = "snapshot_ts,user_id,agent_email,status_name,status_start_utc,status_duration,routing_profile,contacts",
    order_by = "agent_email.asc"
  )
  if (nrow(df) == 0) return(data.frame())
  df |> mutate(
    platform = "AWS",
    snapshot_ts = ymd_hms(snapshot_ts, tz = "UTC"),
    status_start_utc = ymd_hms(status_start_utc, tz = "UTC")
  )
}

#' @export
read_latest_five9 <- function() {
  df <- read_latest_snapshot(
    table = "five9_agent_snapshots",
    select = "snapshot_ts,username,full_name,state,reason_code,state_since_utc,state_duration",
    order_by = "username.asc"
  )
  if (nrow(df) == 0) return(data.frame())
  df |> mutate(
    platform = "Five9",
    agent_email = username,
    status_name = case_when(
      state == "Ready"           ~ "Available",
      state == "Not Ready" & reason_code != "" & !is.na(reason_code) ~ reason_code,
      state == "Not Ready"       ~ "Not Ready",
      state == "On Call"         ~ "On Call",
      state == "After Call Work" ~ "ACW",
      TRUE                       ~ state
    ),
    snapshot_ts = ymd_hms(snapshot_ts, tz = "UTC"),
    status_start_utc = ymd_hms(state_since_utc, tz = "UTC"),
    status_duration = as.integer(state_duration) %/% 1000L,
    routing_profile = NA_character_,
    contacts = NA_character_
  ) |>
    select(platform, snapshot_ts, agent_email, status_name,
           status_start_utc, status_duration, routing_profile, contacts)
}

# ── Read pre-computed agent intervals ────────────────────────
#' @export
read_agent_intervals <- function(target_date) {
  if (SUPABASE_KEY == "") stop("Set SUPABASE_KEY env var")

  all_rows <- list()
  offset <- 0
  page_size <- 1000

  repeat {
    resp <- GET(
      paste0(SUPABASE_URL, "/rest/v1/agent_intervals"),
      query = list(
        select = "date,interval,agent_email,platform,status_name,seconds",
        date = paste0("eq.", target_date),
        order = "agent_email.asc,interval.asc",
        offset = offset,
        limit = page_size
      ),
      add_headers(
        apikey = SUPABASE_KEY,
        Authorization = paste("Bearer", SUPABASE_KEY)
      )
    )

    if (status_code(resp) != 200) {
      stop(paste("Supabase error:", status_code(resp), content(resp, as = "text")))
    }

    page <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    if (is.null(page) || nrow(page) == 0) break

    all_rows[[length(all_rows) + 1]] <- page
    if (nrow(page) < page_size) break
    offset <- offset + page_size
  }

  if (length(all_rows) == 0) return(data.frame())

  long_df <- bind_rows(all_rows)

  # Pivot wide: one column per status
  long_df |>
    pivot_wider(
      id_cols = c(agent_email, platform, date, interval),
      names_from = status_name,
      values_from = seconds,
      values_fill = 0
    ) |>
    arrange(agent_email, interval)
}

# ── Write (upsert) agent intervals to Supabase ──────────────
#' @export
write_agent_intervals <- function(df) {
  if (SUPABASE_KEY == "") stop("Set SUPABASE_KEY env var")
  if (nrow(df) == 0) return(invisible(NULL))

  # df should be long-format: date, interval, agent_email, platform, status_name, seconds
  # Upsert in batches of 500
  batch_size <- 500
  n_batches <- ceiling(nrow(df) / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(df))
    batch <- df[start_idx:end_idx, ]

    payload <- toJSON(batch, auto_unbox = TRUE, dataframe = "rows")

    resp <- POST(
      paste0(SUPABASE_URL, "/rest/v1/agent_intervals"),
      body = payload,
      encode = "raw",
      content_type = "application/json",
      add_headers(
        apikey = SUPABASE_KEY,
        Authorization = paste("Bearer", SUPABASE_KEY),
        Prefer = "resolution=merge-duplicates"
      )
    )

    if (status_code(resp) >= 300) {
      warning(paste("[COMPUTE] Upsert batch", i, "failed:", status_code(resp),
                    content(resp, as = "text")))
    }
  }

  message(paste("[COMPUTE] Upserted", nrow(df), "rows to agent_intervals"))
  invisible(NULL)
}
