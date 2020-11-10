#' @export
aria_server <- function(version = "test"){

  function(input, output, session){
    # close the R session when Chrome closes
    session$onSessionEnded(function() {
      if(version == "desktop"){
        stopApp()
        q("no")
      }
    })
    
    comparison <- reactiveValues(x = FALSE)
    aria_values <- reactiveValues(temp_dir = tempdir(),
                                  download_count = 0,
                                  session_id = generate_session_id(),
                                  rfept = 0, sistec = 0,
                                  first_run = TRUE)

    # delete files and reload the app after download ARIA output
    observeEvent(aria_values$download_count, {
      unlink(paste0(aria_values$temp_dir, "/", aria_values$session_id), recursive = TRUE)
      comparison$x <- FALSE
      session$reload()
    }, ignoreInit = TRUE)
    
    output$download_online <- downloadHandler(
      filename = "ARIA.zip",
      content = function(file){
        # go to a temp dir to avoid permission issues

        if(isolate(aria_values$download_count == 0)){
          owd <- setwd(aria_values$temp_dir)
          on.exit(setwd(owd))
          files <- NULL;
          
          write_output(x = isolate(comparison$x),
                       output_path = paste0(aria_values$temp_dir, "/", aria_values$session_id), 
                       output_folder_name = "ARIA")
          
          aria_values$download_count <- 1
          
          # create the zip file
          system(paste0("cd ", aria_values$session_id, "; zip -r ",
                        file, " ARIA"))
        } else {
          session$reload()
        }

       # utils::zip(file, "ARIA/")
      }
    )
    
    output$contents <- renderText({

      aria_logs(aria_values$session_id,
                input$rfept$datapath[1],
                input$sistec$datapath[1],
                comparison$x,
                aria_values$temp_dir,
                aria_values$download_count)

      if(is.null(input$do)){
        comparison$x <- FALSE
      } else if(isolate(input$do == 0)){
        comparison$x <- FALSE
      } else {
        comparison$x <- isolate(
          shiny_comparison(input$sistec$datapath[1],
                           input$rfept$datapath[1],
                           input$year)
        )
      }
      
      isolate(output_screen(input$sistec$datapath[1],
                            input$rfept$datapath[1],
                            comparison$x)
      )
    })
    
    output$compare_button <- renderUI({
      if(all(!is.null(input$sistec$datapath[1]),
             !is.null(input$rfept$datapath[1]))){
        
        aria_compare_button()
      }
    })
    
    output$download_button <- renderUI({
      
      if(!is.null(input$do)){
        if(input$do != 0){
          aria_download_button()
        }
      }
    })
    
    
  }
}