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
                 version = "offline"){

  shiny_max_file_size <- as.integer(max_file_size*1024^2)
  opt <- options(shiny.maxRequestSize = shiny_max_file_size) 
  on.exit(options(opt))

  period_input <- read_period_input()
  
  ui <- fluidPage(
    navbarPage(paste0("ARIA v", aria_version()),
               tabPanel("SISTEC",
                        sidebarLayout(
                          sidebarPanel(
                            tags$head(tags$style(aria_head_tags())),
                            aria_input_rfept(),
                            aria_input_sistec(),
                            aria_input_years(period_input$PERIOD),
                            br(),
                            aria_input_compare_button(),
                            aria_input_download_button(),
                            aria_test_mode_checkbox(test_mode)
                          ),
                         aria_main_panel(version)
                        )
               ),
               tabPanel("MANUAL", manual_screen())
    )
  )
 
  server <- function(input, output, session){
    # close the R session when Chrome closes
    session$onSessionEnded(function() {
      if(is.null(isolate(input$test_mode))){
        stopApp()
        q("no")
      } else {
        stopApp()
      }
    })

    comparison <- reactiveValues(x = FALSE)
    
    output$download_online <- downloadHandler(
      filename = "ARIA.zip",
      content = function(file){
        #go to a temp dir to avoid permission issues
        output_path <- tempdir()
        owd <- setwd(output_path)
        on.exit(setwd(owd))
        files <- NULL;
        
        if(is.list(isolate(comparison$x))){
          
          output_path <- shiny_output_path(output_path)
          
          write_output(x = isolate(comparison$x),
                       output_path = output_path,
                       output_folder_name = output_folder_name)
          
          "Download realizado com sucesso!"
          
        } else {
          ""
        }
        #create the zip file
        utils::zip(file, "ARIA/")
      }
    )
    
    output$download_offline <- renderText({

      if(is.null(input$download_offline)) return()
      if(input$download_offline == 0) return()

      if(is.list(isolate(comparison$x))){
        
        output_path <- shiny_output_path(output_path)
        
        write_output(x = isolate(comparison$x),
                     output_path = output_path,
                     output_folder_name = output_folder_name)
        
        "Download realizado com sucesso!"
      } else {
        "" 
      }
    })
    
    output$contents <- renderText({

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
          aria_download_button(version)
        }
      } 
    })
    
    
  }
  
  aria_run(ui, server, version, options_port, options_launch_browser)
} 
