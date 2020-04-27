server_input_path <- function(input_path){
  slash <- stringr::str_locate_all(input_path[1], "/")
  last_slash <- slash[[1]][nrow(slash[[1]]), 2]
  substr(input_path[1], 1, last_slash)
}

shiny_comparison <- function(sistec_path, qacademico_path){
  if(!is.null(sistec_path) && !is.null(qacademico_path)){
    sistec_path <- server_input_path(sistec_path)
    qacademico_path <- server_input_path(qacademico_path)
    compare_sistec(sistec_path, qacademico_path)  
  } else {
    FALSE
  }
}