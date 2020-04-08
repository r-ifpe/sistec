#' @export
compare_sistec <- function (x, y, type = "complete") {
  UseMethod("compare_sistec", x)
}

compare_sistec.sistec_complete <- function(x, y, type = "complete"){
  
}

compare_sistec.sistec_simplified <- function(x, y, type = "complete"){
  
}

compare_sistec.character <- function(x, y, type = "complete"){
  
}

