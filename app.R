
library(shiny)

ui <- fluidPage(
    navbarPage("Sistec_app v0.0.1",
               tabPanel("Qacademico",
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Escolha os arquivos",
                      multiple = TRUE,
                      buttonLabel = "Arquivos",
                      placeholder = "Nada Selecionado",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv",
                          ".xlsx")
            ),
            tags$head(tags$script(src = "message-handler.js")),
            actionButton("do", "Comparar")
        ),
        mainPanel(
            p(""),
            p(""),
            strong(textOutput("contents")))
    )
               )
    )

)

server <- function(input, output, session) {
    # close the R session when Chrome closes
    # session$onSessionEnded(function() {
    #     stopApp()
    #     q("no")
    # })

    output$contents <- renderText({
        
        if (input$do == 0){
            "Selecione os arquivos do Qacademico e Sistec."
        } else {
            if(is.null(input$file1)){
             "Selecione ao menos um arquivo de cada sistema."   
            } else {
                slash <- stringr::str_locate_all(input$file1$datapath[1], "/")
                last_slash <- slash[[1]][nrow(slash[[1]]), 2]
                path <- substr(input$file1$datapath[1], 1, last_slash)
                
                source("comparar_qacademico.R", encoding = "UTF-8")
                comparar_q_sistec(path = path)
                "Comparação entre Qacademico e Sistec realizada com sucesso!"   
            }
        }
     })
}

shinyApp(ui, server)
