num_para_cpf <- function(cpf) {
  
  cpf <- stringr::str_pad(cpf, 11, pad = "0")
  stringr::str_replace(string = cpf,
                       pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
                       replacement = "\\1.\\2.\\3-")
}

co_unidade_ensino <- function(){
  utils::read.csv(system.file("extdata/co_unidade_ensino/co_unidade_ensino.csv", package = "sistec"),
                              colClasses = "character")
}


detect_sep <- function(x){
  header <- readLines(x, n = 1, encoding = "UTF-8")
  comma_sep <- stringr::str_detect(header, ",")
  if(comma_sep){
    sep = ","
  } else {
    sep = ";"
  }
  
  sep
}
