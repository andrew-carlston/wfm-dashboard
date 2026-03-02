# ── Supabase snapshot readers ──────────────────────────────────
# Shared data layer for reading agent snapshots from both platforms.

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

SUPABASE_URL <- Sys.getenv("SUPABASE_URL", "https://cfiozaadhfmgliffrdvh.supabase.co")
SUPABASE_KEY <- Sys.getenv("SUPABASE_KEY", "")

# ── Generic paginated Supabase reader ────────────────────────
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

  df %>%
    mutate(
      platform = "AWS",
      snapshot_ts = ymd_hms(snapshot_ts, tz = "UTC"),
      status_start_utc = ymd_hms(status_start_utc, tz = "UTC")
    )
}

# ── Five9 snapshots ──────────────────────────────────────────
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

  df %>%
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
    ) %>%
    select(platform, snapshot_ts, agent_email, status_name,
           status_start_utc, status_duration, routing_profile, contacts)
}

# ── Latest snapshot only (for live status tab) ───────────────
read_latest_snapshot <- function(table, select, order_by) {
  if (SUPABASE_KEY == "") stop("Set SUPABASE_KEY env var")

  # Get the max snapshot_ts first
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

  # Read all rows for that snapshot_ts
  read_supabase_table(
    table = table,
    select = select,
    filter_col = "snapshot_ts",
    cutoff = latest_ts,
    order_by = order_by
  )
}

read_latest_aws <- function() {
  df <- read_latest_snapshot(
    table = "aws_agent_snapshots",
    select = "snapshot_ts,user_id,agent_email,status_name,status_start_utc,status_duration,routing_profile,contacts",
    order_by = "agent_email.asc"
  )
  if (nrow(df) == 0) return(data.frame())
  df %>% mutate(
    platform = "AWS",
    snapshot_ts = ymd_hms(snapshot_ts, tz = "UTC"),
    status_start_utc = ymd_hms(status_start_utc, tz = "UTC")
  )
}

read_latest_five9 <- function() {
  df <- read_latest_snapshot(
    table = "five9_agent_snapshots",
    select = "snapshot_ts,username,full_name,state,reason_code,state_since_utc,state_duration",
    order_by = "username.asc"
  )
  if (nrow(df) == 0) return(data.frame())
  df %>% mutate(
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
  ) %>%
    select(platform, snapshot_ts, agent_email, status_name,
           status_start_utc, status_duration, routing_profile, contacts)
}
