#' Read sistec and qacademico files
#'
#' These functions support two kinds of schemas: from the api and from the website.
#'
#' @param path The sistec file's path. 
#' @param type Choose "simplified" when you download directly from the website and you just want to 
#' compare an specific amount of ciclos. Choose "complete" when you download from the API and you want to 
#' compare from the whole institute.
#' @name read_files
NULL

#' @rdname read_files
#' @importFrom rlang sym
#' @export
read_sistec <- function(path = "extdata", type = "simplified"){
  
  # website
  if(type == "simplified"){
    read_sistec_simplified(path) # from the website
  } else if(type == "complete"){
    read_sistec_complete(path) # from the api
  } else{
    stop("The type is wrong. Choose simplified or complete")
  }
}

#' @rdname read_files
#' @importFrom rlang sym
#' @export
read_qacademico <- function(path = "extdata", type = "simplified"){
  
  # website
  if(type == "simplified"){
    read_qacademico_simplified(path) # from the website
  } else if(type == "complete"){
    read_qacademico_complete(path) # from the api
  } else{
    stop("The type is wrong. Choose simplified or complete")
  }
}

#' @importFrom rlang sym syms
read_qacademico_complete <- function(path){
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  # Matrícula, Situação.Matrícula, Situação.Período, Instituição
  
  vars <- c("Matr\u00edcula", "Nome", "Situa\u00e7\u00e3o.Matr\u00edcula",
            "Situa\u00e7\u00e3o.Per\u00edodo", "Curso", "Cpf",
            "Institui\u00e7\u00e3o", "Per..Letivo.Inicial")
  
  classes <- c(Cpf = "character")
  
  lapply(temp, function(e){
    utils::read.csv(e, sep = "",  stringsAsFactors = FALSE, 
                    encoding = "latin1", colClasses = classes) %>% 
        dplyr::select(!!!syms(vars))
    }) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(Cpf= num_para_cpf(!!sym("Cpf"))) %>% 
    dplyr::distinct(!!sym("Matr\u00edcula"), .keep_all = TRUE)
}

read_qacademico_simplified <- function(path){
  temp <-  list.files(path = path, pattern = "*.xlsx")
    temp <- paste0(path , "/", temp)
    lapply(temp, openxlsx::read.xlsx) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(Cpf= num_para_cpf(!!sym("Cpf")))
}

#' @importFrom dplyr %>% 
#' @importFrom rlang sym
read_sistec_complete <- function(path){
  
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)
  
  classes <- c(Numero.Cpf = "character")
  
  lapply(temp, utils::read.csv,
         sep = ";",  stringsAsFactors = FALSE, 
         colClasses = classes, encoding = "UTF-8") %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Numero.Cpf = ifelse(stringr::str_length(!!sym("Numero.Cpf")) == 0, 
                                      !!sym("Numero.Cpf"), # in API number zero in the beguinning is blank
                                      stringr::str_pad(!!sym("Numero.Cpf"), 11, pad = "0"))) %>% 
    dplyr::transmute(NO_ALUNO = !!sym("Nome.Aluno"), 
                     NU_CPF = num_para_cpf(!!sym("Numero.Cpf")),
                     CO_CICLO_MATRICULA = !!sym("Co.Ciclo.Matricula"), 
                     NO_STATUS_MATRICULA = !!sym("Situa\u00e7\u00e3o.Matricula"), # Situação.Matricula
                     NO_CURSO = !!sym("No.Curso"), 
                     DT_DATA_INICIO = !!sym("Dt.Data.Inicio"))
}

#' @importFrom dplyr %>% 
#' @importFrom rlang sym syms
read_sistec_simplified <- function(path){
  
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)
  
  classes <- c(NU_CPF = "character", CO_CICLO_MATRICULA = "character",
               NO_STATUS_MATRICULA = "character")
  
  sistec_vars <- c("NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA", "NO_STATUS_MATRICULA")

  lapply(temp, utils::read.csv,
         sep = ";",  stringsAsFactors = FALSE, 
         colClasses = classes, encoding = "latin1") %>%
    dplyr::bind_rows() %>% 
    dplyr::mutate(NU_CPF =num_para_cpf(!!sym("NU_CPF"))) %>% 
    dplyr::select(!!!syms(sistec_vars))
}

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
  browser()
  if(stringr::str_length(cpf)  == 11|stringr::str_length(cpf)  == 0){
    cpf
  } else {
    zeros <- 11 - stringr::str_length(cpf)  # a cpf always have 11 numbers
    zeros <- paste0(zeros, collapse = "")
    paste0(zeros, cpf)
  }
}
