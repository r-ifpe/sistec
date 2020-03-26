library(shiny)
library(sistec)

ui <- fluidPage(
    navbarPage("Sistec_app v0.0.1.9001",
               tabPanel("Qacademico",
    sidebarLayout(
        sidebarPanel(
            fileInput("qacademico", "Escolha os arquivos do qacademico",
                      multiple = TRUE,
                      buttonLabel = "Arquivos",
                      placeholder = "Nada Selecionado",
                      accept = c(".xlsx")
            ),
            fileInput("sistec", "Escolha os arquivos do sistec",
                      multiple = TRUE,
                      buttonLabel = "Arquivos",
                      placeholder = "Nada Selecionado",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")),
            actionButton("do", "Comparar")

        ),
        mainPanel(
            p(""),
            p(""),
            strong(htmlOutput("contents")))
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

    output$contents <- renderText({

        input$do
        isolate(sistec::output_screen(input$qacademico$datapath[1],
                                      input$sistec$datapath[1])
            
        )

     })
}

shinyApp(ui, server)
