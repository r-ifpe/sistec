#' @importFrom shiny NS
input_rfept <- function(id) {
  shiny::fileInput(NS(id, "rfept"), "Escolha os arquivos do sistema acad\u00eamico",
    multiple = TRUE,
    buttonLabel = "Arquivos",
    placeholder = "Nada Selecionado",
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  )
}

#' @importFrom shiny NS
input_sistec <- function(id) {
  shiny::fileInput(NS(id, "sistec"), "Escolha os arquivos do Sistec",
    multiple = TRUE,
    buttonLabel = "Arquivos",
    placeholder = "Nada Selecionado",
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  )
}

#' @importFrom shiny NS
input_years <- function(id) {
  years <- utils::read.csv(
    system.file("extdata/shiny/period_input.csv", package = "sistec"),
    header = TRUE, colClasses = "character"
  )

  shiny::selectInput(NS(id, "year"), "Comparar a partir de:",
    choices = years$PERIOD,
    selected = "2019.1"
  )
}

#' @importFrom shiny NS
input_compare_button <- function(id) {
  shiny::div(
    style = "display: inline-block;vertical-align:top",
    shiny::uiOutput(NS(id, "compare_button"))
  )
}

#' @importFrom shiny NS
input_download_button <- function(id) {
  shiny::div(
    style = "display: inline-block;vertical-align:top",
    shiny::uiOutput(NS(id, "download_button"))
  )
}
