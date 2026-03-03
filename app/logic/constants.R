# ── Shared constants and helpers ──────────────────────────────

#' @export
STATUS_CONFIG <- list(
  Available   = list(bg = "#28a745", icon = "fa-check-circle",   order = 1),
  `On Call`   = list(bg = "#007bff", icon = "fa-phone-alt",      order = 2),
  ACW         = list(bg = "#ffc107", icon = "fa-clipboard",      order = 3),
  Break       = list(bg = "#fd7e14", icon = "fa-mug-hot",        order = 4),
  Lunch       = list(bg = "#e83e8c", icon = "fa-utensils",       order = 5),
  Training    = list(bg = "#6f42c1", icon = "fa-graduation-cap", order = 6),
  Meeting     = list(bg = "#20c997", icon = "fa-calendar",       order = 7),
  Coaching    = list(bg = "#17a2b8", icon = "fa-chalkboard-teacher", order = 8),
  `Not Ready` = list(bg = "#dc3545", icon = "fa-times-circle",   order = 9),
  Offline     = list(bg = "#6c757d", icon = "fa-power-off",      order = 10)
)

#' @export
get_config <- function(status) {
  if (status %in% names(STATUS_CONFIG)) STATUS_CONFIG[[status]]
  else list(bg = "#6c757d", icon = "fa-question-circle", order = 99)
}

#' @export
fmt_duration <- function(secs) {
  secs <- as.integer(secs)
  hrs <- secs %/% 3600
  mins <- (secs %% 3600) %/% 60
  s <- secs %% 60
  sprintf("%02d:%02d:%02d", hrs, mins, s)
}

#' @export
INFRACTION_POINTS <- list(
  tardy_short        = 1,
  tardy_long         = 2,
  left_early_under50 = 3,
  left_early_over50  = 2,
  ncns               = 5,
  missed_meeting     = 1
)

#' @export
POINT_THRESHOLDS <- data.frame(
  points = c(6, 8, 10, 12),
  action = c("Verbal Warning", "Written Warning", "Final Warning", "Termination"),
  floor  = c(4, 6, 8, NA),
  stringsAsFactors = FALSE
)

#' @export
PRODUCTIVE_STATES <- c("Available", "Ready", "On Call", "On call", "ACW",
                        "After Call Work", "Chat", "Outbound")

#' @export
APPROVED_STATES <- c("Break", "Lunch", "Meeting", "Training", "Coaching")
