library(shiny)
library(sistec)
options(shiny.maxRequestSize = 100*1024^2) 

aria_aws(test_mode = FALSE)
# 
# 
# ###################
# library(shiny)
# options(shiny.maxRequestSize = 100*1024^2) 
# 
# ui <- fluidPage(
#     navbarPage("Sistec_app v0.0.1.9008",
#                tabPanel("Qacademico",
#     sidebarLayout(
#         sidebarPanel(
# 
#             fileInput("qacademico", "Escolha os arquivos do Qacademico",
#                       multiple = TRUE,
#                       buttonLabel = "Arquivos",
#                       placeholder = "Nada Selecionado",
#                       accept = c(".xlsx")
#             ),
#             fileInput("sistec", "Escolha os arquivos do Sistec",
#                       multiple = TRUE,
#                       buttonLabel = "Arquivos",
#                       placeholder = "Nada Selecionado",
#                       accept = c(
#                           "text/csv",
#                           "text/comma-separated-values,text/plain",
#                           ".csv")),
#             actionButton("do", "Comparar"),
#             actionButton("download", "Salvar resultados"),
#             if(FALSE){
#                 selectInput("test", "Test mode", TRUE)
#             }
# 
#         ),
#         mainPanel(
#             strong(htmlOutput("contents")),
#             br(), br(), br(), br(),
#             strong(htmlOutput("download"))
#             )
#     )
#                )
#     )
# 
# )
# 
# server <- function(input, output, session){
#    # close the R session when Chrome closes
#     session$onSessionEnded(function() {
#         if(is.null(isolate(input$test))){
#             stopApp()
#             q("no")
#         }
#     })
#     
# 
#     comparison <- reactiveValues(x = FALSE)
#     
#     output$download <- renderText({
#         input$download
# 
#         if(is.list(isolate(comparison$x))){
#             output_path <- choose.dir()
#             output_path <- gsub("\\\\", "/",output_path)
#             sistec:::write_output(output_path = output_path,
#                                   output_folder_name = "Sistec_app",
#                                   comparison = isolate(comparison$x))
#             
#             "Download realizado com sucesso!"
#             
#         } else {
#            "" 
#         }
#         
#     })
# 
#     output$contents <- renderText({
# 
#         input$do
#         
#         comparison$x <- isolate(
#             if(!is.null(input$sistec$datapath[1]) && !is.null(input$qacademico$datapath[1])){
#                 sistec:::shiny_comparison(input$sistec$datapath[1],
#                                           input$qacademico$datapath[1])
#             } else {FALSE}
#         ) 
# 
#         isolate(sistec:::output_screen(input$qacademico$datapath[1],
#                                        input$sistec$datapath[1],
#                                        comparison$x)
#             
#         )
# 
#      })
# }
# 
# shinyApp(ui, server)
