# ── WFM Dashboard ─────────────────────────────────────────────
# bs4Dash app: Live Agent Status, 15-Min Interval Grid, Adherence Report
# Data: Supabase (aws_agent_snapshots + five9_agent_snapshots)

library(shiny)
library(bs4Dash)

source("R/mod_live_status.R")
source("R/mod_interval_grid.R")
source("R/mod_adherence.R")

# ── UI ───────────────────────────────────────────────────────
ui <- dashboardPage(
  dark = FALSE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "WFM Dashboard",
      color = "primary"
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Live Status", tabName = "live", icon = icon("users")),
      menuItem("Interval Grid", tabName = "grid", icon = icon("table")),
      menuItem("Adherence", tabName = "adherence", icon = icon("chart-bar"))
    )
  ),
  body = dashboardBody(
    # Global: kill Shiny's recalculating gray overlay on all outputs
    tags$style(HTML("
      .recalculating { opacity: 1 !important; }
    ")),
    tabItems(
      tabItem(
        tabName = "live",
        live_status_ui("live_status")
      ),
      tabItem(
        tabName = "grid",
        interval_grid_ui("interval_grid")
      ),
      tabItem(
        tabName = "adherence",
        adherence_ui("adherence")
      )
    )
  ),
  footer = dashboardFooter(
    left = "WFM Dashboard v1.0",
    right = paste("LawnStarter", format(Sys.Date(), "%Y"))
  )
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {
  live_status_server("live_status")
  interval_grid_server("interval_grid")
  adherence_server("adherence")
}

# ── Run ──────────────────────────────────────────────────────
shinyApp(ui, server)
