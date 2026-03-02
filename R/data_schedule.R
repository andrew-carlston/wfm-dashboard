# ── Google Sheets schedule reader ─────────────────────────────
# Reads agent shift schedules from a Google Sheet.

library(googlesheets4)
library(gargle)
library(dplyr)
library(lubridate)

# ── Authenticate with service account ────────────────────────
auth_google_sheets <- function() {
  json_str <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
  if (json_str == "") {
    gs4_deauth()
    return(invisible(NULL))
  }

  # Write JSON to temp file for gargle
  tmp <- tempfile(fileext = ".json")
  writeLines(json_str, tmp)
  on.exit(unlink(tmp))

  gs4_auth(path = tmp)
}

# ── Read schedule sheet ──────────────────────────────────────
read_schedule <- function(sheet_id = NULL) {
  if (is.null(sheet_id)) {
    sheet_id <- Sys.getenv("SCHEDULE_SHEET_ID", "")
  }
  if (sheet_id == "") {
    message("No SCHEDULE_SHEET_ID configured")
    return(data.frame())
  }

  auth_google_sheets()

  df <- tryCatch(
    read_sheet(sheet_id, sheet = 1),
    error = function(e) {
      message(paste("Error reading schedule sheet:", e$message))
      return(data.frame())
    }
  )

  if (nrow(df) == 0) return(data.frame())

  # Normalize column names
  names(df) <- tolower(gsub("[^a-z0-9]", "_", tolower(names(df))))

  # Expect columns: agent_email, date, shift_start, shift_end
  required <- c("agent_email", "date", "shift_start", "shift_end")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    message(paste("Schedule sheet missing columns:", paste(missing, collapse = ", ")))
    return(data.frame())
  }

  df %>%
    mutate(
      date = as.Date(date),
      shift_start = hm(shift_start),
      shift_end = hm(shift_end),
      shift_start_min = as.numeric(shift_start) / 60,
      shift_end_min = as.numeric(shift_end) / 60,
      scheduled_min = shift_end_min - shift_start_min
    ) %>%
    select(agent_email, date, shift_start, shift_end,
           shift_start_min, shift_end_min, scheduled_min)
}
