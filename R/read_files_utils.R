
num_para_cpf <- function(num) {
  
  stringr::str_replace(string = num,
                       pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
                       replacement = "\\1.\\2.\\3-")
}

server_input_path <- function(input_path){
  slash <- stringr::str_locate_all(input_path[1], "/")
  last_slash <- slash[[1]][nrow(slash[[1]]), 2]
  substr(input_path[1], 1, last_slash)
}

complete_cpf <- function(cpf){
  if(stringr::str_length(cpf)  == 11|stringr::str_length(cpf)  == 0){
    cpf
  } else {
    zeros <- 11 - stringr::str_length(cpf)  # a cpf always have 11 numbers
    zeros <- paste0(zeros, collapse = "")
    paste0(zeros, cpf)
  }
}
