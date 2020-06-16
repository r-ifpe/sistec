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
                 test_mode = TRUE){

  shiny_max_file_size <- as.integer(max_file_size*1024^2)
  opt <- options(shiny.maxRequestSize = shiny_max_file_size) 
  on.exit(options(opt))
  
  description_path <- system.file("DESCRIPTION", package = "sistec")
  version <- as.character(read.dcf(description_path, fields = "Version"))
  
  period_input <- read_period_input()
  
  ui <- fluidPage(
    navbarPage(paste0("ARIA v", version),
               tabPanel("SISTEC",
                        sidebarLayout(
                          sidebarPanel(
                            fileInput("rfept", "Escolha os arquivos do registro acad\u00eamico",
                                      multiple = TRUE,
                                      buttonLabel = "Arquivos",
                                      placeholder = "Nada Selecionado",
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            fileInput("sistec", "Escolha os arquivos do Sistec",
                                      multiple = TRUE,
                                      buttonLabel = "Arquivos",
                                      placeholder = "Nada Selecionado",
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            selectInput("year", "Comparar a partir de:",
                                        choices = period_input$PERIOD,
                                        selected = "2019.1"),
                            actionButton("do", "Comparar"),
                            actionButton("download", "Salvar resultados"),
                            if(test_mode) checkboxInput("test_mode", "Test mode", TRUE)
                          ),
                          mainPanel(
                            strong(htmlOutput("contents")),
                            br(), br(), br(), br(),
                            strong(htmlOutput("download"))
                          )
                        )
               )
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
    
    output$download <- renderText({
      input$download
      
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
      input$do
      
      comparison$x <- isolate(
        if(!is.null(input$sistec$datapath[1]) && !is.null(input$rfept$datapath[1])){
          shiny_comparison(input$sistec$datapath[1],
                           input$rfept$datapath[1],
                           input$year)
        } else {FALSE}
      ) 
      
      isolate(output_screen(input$sistec$datapath[1],
                            input$rfept$datapath[1],
                            comparison$x)
      )
    })
  }
  
  shinyApp(ui, server, options = list(port = options_port,
                                      launch.browser = options_launch_browser))
} 
