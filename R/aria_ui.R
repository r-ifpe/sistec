#' @rdname aria
#' @export
aria_ui <- function(version = "test") {
  shiny::fluidPage(shiny::navbarPage(
      aria_title_name(),
      sistec_panel(version),
      manual_panel()
  ))
}

aria_title_name <- function(){
  paste0("ARIA v", aria_version())
}

sistec_panel <- function(version){
  shiny::navbarMenu("SISTEC",
    shiny::tabPanel("Compara\u00e7\u00e3o entre Sistec e sistema acad\u00eamico",
      comparison_ui("comparison", version)
    )
  )
}

manual_panel <- function(){
  shiny::tabPanel("MANUAL",
    shiny::includeHTML(system.file(
      "extdata/docs/aria/manual_panel.html", package = "sistec"
    ))
  )
}
