
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
            strong(htmlOutput("contents")))
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
                tabela_comparacao <- comparar_q_sistec(path = path)

                openxlsx::write.xlsx(tabela_comparacao$situacao, "situação.xlsx")
                
                total_students <- nrow(tabela_comparacao$ifpe_dados)
                multi_vinculo <- nrow(tabela_comparacao$situacao$multi_vinculo)
                
                students_to_update <- sum(unlist(
                    lapply(tabela_comparacao$situacao, nrow))) - multi_vinculo
                
                students_updated <- total_students - students_to_update - multi_vinculo

                HTML(paste("Comparação entre Qacademico e Sistec realizada com sucesso!",
                     "", "", "", 
                     paste0("Situações comparadas: ", total_students),
                     paste0("Alunos atualizados: ", students_updated,
                           " (", round(100*students_updated/total_students, 2 ), "%)"),
                     paste("Alunos com mais de um vínculo:", multi_vinculo), sep = '<br/>'))
            }
        }
     })
}

shinyApp(ui, server)
