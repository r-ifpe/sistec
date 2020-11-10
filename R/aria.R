#' Aria web application
#' 
#' This is the web application using the sistec package. It was created to ease the 
#' work using the package, but you can have the same results reading the 
#' files (sistec::read_rfept()), comparing the results with (sistec::compare_sistec()) and write 
#' the outputs (sistec::write_output()).
#' 
#' @param output_path The folder where you want to save the results.
#' @param output_folder_name The folder's name you want to save the results.
#' @param max_file_size The maximum file size in megabytes.
#' @param options_port The TCP port that the application should listen on, usually 8888.
#' @param options_launch_browser If true, the system's default web browser will be 
#' launched automatically after the app is started. 
#' @param test_mode Use FALSE in production and TRUE when you are testing. In production,
#' when you close the browser ,the app and the R session will be closed. In test, only the app will 
#' close when you close the browser.
#' @param version A string. Choose "offline" or "online" version.
#' 
#' @return A web application.
#'
#' @import shiny 
#' @export
aria <- function(output_path = NULL,
                 output_folder_name = "ARIA",
                 max_file_size = 100,
                 options_port = 8888,
                 options_launch_browser = TRUE,
                 test_mode = TRUE,
                 version = "test"){

  shiny_max_file_size <- as.integer(max_file_size*1024^2)
  opt <- options(shiny.maxRequestSize = shiny_max_file_size) 
  on.exit(options(opt))

  ui <- aria_ui()
  server <- aria_server(version)
  aria_run(ui, server, version, options_port, options_launch_browser)
} 
