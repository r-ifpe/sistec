#' Sistec web application
#' 
#' This is the web application using the sistec package. It was created to ease the 
#' work using the package, but you can have the same results reading the 
#' files (sistec::read_*()), comparing the results (sistec::compare_sistec()) and write 
#' the outputs (sistec::write_output())
#' 
#' @param output_path The folder where you want to save the results.
#' @param output_folder_name The folder's name you want to save the results.
#' @param max_file_size The maximum file size in megabytes.
#' 
#' @return A web application.
#'
#' @import shiny
#' @export
sistec_app <- function(output_path = NULL,
                       output_folder_name = "Sistec_app",
                       max_file_size = 100){
  
  opt <- options(shiny.maxRequestSize = max_file_size*1024^2) 
  on.exit(options(opt))
  
  description_path <- system.file("DESCRIPTION", package = "sistec")
  version <- as.character(read.dcf(description_path, fields = "Version"))
  
  ui <- fluidPage(
    navbarPage(paste0("Sistec_app v", version),
               tabPanel("Qacademico",
                        sidebarLayout(
                          sidebarPanel(
                            fileInput("qacademico", "Escolha os arquivos do Qacademico",
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
                            actionButton("do", "Comparar"),
                            actionButton("download", "Salvar resultados")
                            
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
    # # close the R session when Chrome closes
    #  session$onSessionEnded(function() {
    #      stopApp()
    #      q("no")
    #  })
    comparison <- reactiveValues(x = FALSE)
    
    output$download <- renderText({
      input$download
      
      if(is.list(isolate(comparison$x))){
        
        output_path <- shiny_output_path(output_path)

        write_output(output_path = output_path,
                     output_folder_name = output_folder_name,
                     comparison = isolate(comparison$x))
        
        "Download realizado com sucesso!"
        
      } else {
        "" 
      }
    })
    
    output$contents <- renderText({
      input$do
      
      comparison$x <- isolate(
        if(!is.null(input$sistec$datapath[1]) && !is.null(input$qacademico$datapath[1])){
          shiny_comparison(input$sistec$datapath[1],
                           input$qacademico$datapath[1])
        } else {FALSE}
      ) 
      
      isolate(output_screen(input$qacademico$datapath[1],
                            input$sistec$datapath[1],
                            comparison$x)
      )
    })
  }
  
  shinyApp(ui, server)
} 
