#' @importFrom shiny tags br
comparison_ui <- function(id, version) {
  shiny::sidebarLayout(shiny::sidebarPanel(
      tags$head(tags$style(aria_head_tags())),
      input_rfept(id),
      input_sistec(id),
      input_years(id),
      br(),
      input_compare_button(id),
      input_download_button(id)
    ),
    comparison_main_panel(id, version)
  )
}

#' @importFrom shiny strong br NS
comparison_main_panel <- function(id, version) {
  if (version == "desktop") {
    shiny::mainPanel(
      strong(shiny::htmlOutput(NS(id, "comparison_main_screen"))),
      br(), br(), br(), br(),
      strong(shiny::htmlOutput(NS(id, "download_offline")))
    )
  } else {
    shiny::mainPanel(
      strong(shiny::htmlOutput(NS(id, "comparison_main_screen")))
    )
  }
}
