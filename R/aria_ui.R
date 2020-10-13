aria_input_rfept <- function(){
  shiny::fileInput("rfept", "Escolha os arquivos do sistema acad\u00eamico",
                   multiple = TRUE,
                   buttonLabel = "Arquivos",
                   placeholder = "Nada Selecionado",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"))
}

aria_input_sistec <- function(){
  shiny::fileInput("sistec", "Escolha os arquivos do Sistec",
                   multiple = TRUE,
                   buttonLabel = "Arquivos",
                   placeholder = "Nada Selecionado",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"))
}

aria_input_years <- function(years){
  shiny::selectInput("year", "Comparar a partir de:",
                     choices = years,
                     selected = "2019.1")
}

aria_compare_button <- function(){
  shiny::actionButton("do", "Comparar")
}

#' @importFrom shiny mainPanel strong br htmlOutput
aria_main_panel <- function(version){
  if(version == "online"){
    mainPanel(
      strong(htmlOutput("contents")))
  } else{
    mainPanel(
      strong(htmlOutput("contents")),
      br(), br(), br(), br(),
      strong(htmlOutput("download_offline")))
  }
}

aria_download_button <- function(version){
  if(version == "online"){
    shiny::downloadButton("download_online", "Salvar resultados")
  } else {
    shiny::actionButton("download_offline", "Salvar resultados")
  }
}

aria_test_mode_checkbox <- function(test_mode){
  if(test_mode){
    shiny::checkboxInput("test_mode", "Test mode", TRUE)
  } 
}

