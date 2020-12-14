#' Aria web application
#'
#' This is the web application using the sistec package. It was created to ease the
#' work using the package, but you can have the same results reading the
#' files (`read_rfept()`), comparing the results with (`compare_sistec()`) and write
#' the outputs (`write_output()`).
#'
#' @param max_file_size The maximum file size in megabytes.
#' @param options_port The TCP port that the application should listen on, usually 8888.
#' @param options_launch_browser If true, the system's default web browser will be
#' launched automatically after the app is started.
#' @param version Use "test" to run locally, "online" to run in the server
#' or "desktop" to build the desktop version.
#' @param logs Use FALSE if you don't want to retrieve the logs in your R session.
#'
#' @return A web application.
#'
#' @examples
#' \dontrun{
#' library(sistec)
#'
#' # use this to run aria in your R environment
#' aria()
#'
#' # use this if you want to run the aria in a server
#' ui <- aria_ui(version = "online")
#' server <- aria_server(version = "online")
#' shiny::shinyApp(ui, server)
#' }
#'
#' @name aria
NULL

#' @rdname aria
#' @export
aria <- function(max_file_size = 100,
                 options_port = 8888,
                 options_launch_browser = TRUE,
                 version = "test",
                 logs = TRUE) {
  shiny_max_file_size <- as.integer(max_file_size * 1024^2)
  opt <- options(shiny.maxRequestSize = shiny_max_file_size)
  on.exit(options(opt))

  ui <- aria_ui(version)
  server <- aria_server(version, logs)
  aria_run(ui, server, version, options_port, options_launch_browser)
}
