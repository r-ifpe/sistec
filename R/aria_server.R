#' @rdname aria
#' @export
aria_server <- function(version = "test", logs = TRUE){
  function(input, output, session) {
    comparison_server("comparison")
  }
}
