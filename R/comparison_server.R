comparison_server <- function(id, version = "test", logs = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    
    session$onSessionEnded(function() {
      if (version == "desktop") {
        shiny::stopApp()
        q("no")
      }
    })

    # delete files and reload the app after download ARIA output
    shiny::observeEvent(comparison_values$download_count,
      {
        delete_aria_files(comparison_values)
        comparison_values$comparison <- FALSE
        session$reload()
      },
      ignoreInit = TRUE
    )

    comparison_values <- shiny::reactiveValues(
      comparison = FALSE,
      version = version,
      logs = logs,
      session_id = generate_session_id(),
      temp_dir = tempdir(),
      download_count = 0
    )

    output$download_online <- shiny::downloadHandler(
      filename = "ARIA.zip",
      content = function(file) {
        if (isolate(comparison_values$download_count == 0)) {
          comparison_values$download_count <- 1
          create_zipped_file(file, comparison_values)
        } else {
          session$reload()
        }
      }
    )
    
    output$download_offline <- shiny::renderText({
      input$download_offline
      aria_desktop_download_files(isolate(comparison_values))
    })

    output$comparison_main_screen <- shiny::renderText({
      comparison_logs(
        comparison_values,
        input$sistec$datapath[1],
        input$rfept$datapath[1]
      )

      comparison_values$comparison <- execute_comparison(
        input$compare_button,
        input$sistec$datapath[1],
        input$rfept$datapath[1],
        isolate(input$year)
      )

      comparison_output_screen(
        comparison_values$comparison,
        input$sistec$datapath[1],
        input$rfept$datapath[1]
      )
    })

    output$compare_button <- shiny::renderUI({
      active_compare_button(id, input$sistec$datapath[1], input$rfept$datapath[1])
    })

    output$download_button <- shiny::renderUI({
      active_download_button(id, version, input$compare_button)
    })
  })
}

execute_comparison <- function(compare_button,
                               sistec_path,
                               rfept_path,
                               year) { 
  if (is.null(compare_button)) {
    comparison <- FALSE
  } else if (compare_button == 0) {
    comparison <- FALSE
  } else {
    sistec <- read_sistec(server_input_path(sistec_path), year)
    rfept <- read_rfept(server_input_path(rfept_path), year)
    comparison <- compare_sistec(sistec, rfept)
  }

  comparison
}

comparison_logs <- function(comparison_values,
                            sistec_path,
                            rfept_path) {
  if (comparison_values$logs == TRUE) {
    aria_logs(
      comparison_values$session_id,
      rfept_path,
      sistec_path,
      comparison_values$comparison,
      comparison_values$temp_dir,
      comparison_values$download_count
    )
  }
}
