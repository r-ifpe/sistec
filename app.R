
library(shiny)

ui <- fluidPage(
    tags$head(tags$script(src = "message-handler.js")),
    actionButton("do", "Comparar")
)

server <- function(input, output, session) {
    # close the R session when Chrome closes
    session$onSessionEnded(function() {
        stopApp()
        q("no")
    })

    observeEvent(input$do, {

        source("comparar_qacademico.R",encoding = "UTF-8")
        comparar_q_sistec()
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Thank you for clicking')
    })
}

shinyApp(ui, server)
