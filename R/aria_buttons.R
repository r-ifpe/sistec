#' @importFrom shiny NS
active_compare_button <- function(id, sistec_path, rfept_path) {
  if (all(!is.null(sistec_path), !is.null(rfept_path))) {
    shiny::actionButton(NS(id, "compare_button"), "Comparar")
  }
}

#' @importFrom shiny NS
active_download_button <- function(id, version, compare_button_count) {
  if (!is.null(compare_button_count)) {
    if (compare_button_count != 0) {
      if (version == "desktop") {
        shiny::actionButton(NS(id, "download_offline"), "Salvar resultados")
      } else {
        shiny::downloadButton(NS(id, "download_online"), "Salvar resultados")
      }
    }
  }
}
