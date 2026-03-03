# ── Main app: navbarPage with 3 tab modules ──────────────────

box::use(
  shiny[...],
  app/view/live_status,
  app/view/interval_grid,
  app/view/adherence,
  app/logic/compute_intervals[start_interval_job],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    title = "WFM Dashboard",
    id = ns("tabs"),
    theme = bslib::bs_theme(version = 5),
    header = tags$style(HTML(".recalculating { opacity: 1 !important; }")),
    tabPanel("Live Status",
      value = "live",
      live_status$ui(ns("live_status"))
    ),
    tabPanel("Interval Grid",
      value = "grid",
      interval_grid$ui(ns("interval_grid"))
    ),
    tabPanel("Adherence",
      value = "adherence",
      adherence$ui(ns("adherence"))
    ),
    footer = tags$div(
      style = "text-align: center; padding: 10px; color: #999; font-size: 12px;",
      paste("WFM Dashboard v2.0 |", "LawnStarter", format(Sys.Date(), "%Y"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Start background interval computation
    start_interval_job()

    # Wire view modules
    live_status$server("live_status")
    interval_grid$server("interval_grid", active_tab = reactive(input$tabs))
    adherence$server("adherence")
  })
}
