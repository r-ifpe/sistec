#' Read sistec and qacademico files
#'
#' These functions support two kinds of schemas: from the api and from the website.
#'
#' @param path The sistec file's path. 
#' @name read_files
NULL

#' @rdname read_files
#' @importFrom rlang sym
#' @export
read_sistec <- function(path = ""){
  
  if(path == "") stop("You need to specify the path.")
  
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)
  
  vars_setec <- c("Nome Aluno", "Numero Cpf", "Co Ciclo Matricula", 
                  "Situa\u00e7\u00e3o Matricula", "No Curso", "Dt Data Inicio",
                  "Unidade Ensino")
  
  vars_web <- c("NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA", 
                "NO_STATUS_MATRICULA", "NO_CICLO_MATRICULA", "DT_DATA_INICIO",
                "CO_UNIDADE_ENSINO")
  
  sistec <- utils::read.csv(temp[1], sep = ";",  stringsAsFactors = FALSE, 
                            encoding = "UTF-8", check.names = FALSE, nrows = 1)
  
  num_vars_setec <- sum(names(sistec) %in% vars_setec)
  num_vars_web <- sum(names(sistec) %in% vars_web)
  
  if(num_vars_setec > 0){
    if(num_vars_setec < 7){ 
      stop(paste("Not found:",
                 paste(vars_setec[!vars_setec %in% names(sistec)], collapse = ", ")))
      
    } else{
      sistec <- read_sistec_setec(path)
    }
  } else if(num_vars_web > 0){
    if(num_vars_web < 7){ 
      stop(paste("Not found:",
                 paste(vars_web[!vars_web %in% names(sistec)], collapse = ", ")))
      
    } else{
      sistec <- read_sistec_web(path)
    }
  } else {
    stop("Not found Sistec variables in your file.")
  }

  sistec
}

#' @rdname read_files
#' @importFrom rlang sym
#' @export
read_qacademico <- function(path = ""){
  
  if(path == "") stop("You need to specify the path.")
  
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  qacademico <- utils::read.csv(temp[1], sep = "",  stringsAsFactors = FALSE, 
                                encoding = "latin1", nrows = 1, check.names = FALSE)
  
  vars <- c("Matr\u00edcula", "Nome", "Situa\u00e7\u00e3o Matr\u00edcula",
            "Curso", "Cpf", "Institui\u00e7\u00e3o", "Per. Letivo Inicial")
  
  if(sum(names(qacademico) %in% vars) == 7){
    read_qacademico_web(path)
  } else{
    stop(paste("Not found:",
               paste(vars[!vars %in% names(qacademico)], collapse = ", ")))
  }
}

#' @importFrom rlang sym syms
read_qacademico_web <- function(path){
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  # Matrícula, Situação.Matrícula, Situação.Período, Instituição
  vars <- c("Matr\u00edcula", "Nome", "Situa\u00e7\u00e3o.Matr\u00edcula",
            "Curso", "Cpf", "Institui\u00e7\u00e3o", "Per..Letivo.Inicial")
  
  classes <- c(Cpf = "character")
  
  qacademico <- lapply(temp, function(e){
    utils::read.csv(e, sep = "",  stringsAsFactors = FALSE, 
                    encoding = "latin1", colClasses = classes) %>% 
        dplyr::select(!!!syms(vars))
    }) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(Cpf = num_para_cpf(!!sym("Cpf")),
                  Campus = stringr::str_sub(!!sym("Institui\u00e7\u00e3o"),8)) %>% 
    dplyr::distinct(!!sym("Matr\u00edcula"), .keep_all = TRUE) %>% # I found this problem 
    dplyr::mutate(Campus = ifelse(!!sym("Campus") == "", "SEM CAMPUS", !!sym("Campus"))) 
  
  class(qacademico) <- c(class(qacademico), "qacademico_data_frame")

  qacademico
}

# # Deve ser remolvido do pacote
# read_qacademico_simplified <- function(path){
#   temp <-  list.files(path = path, pattern = "*.xlsx")
#     temp <- paste0(path , "/", temp)
#     lapply(temp, openxlsx::read.xlsx) %>%
#       dplyr::bind_rows() %>%
#       dplyr::mutate(Cpf= num_para_cpf(!!sym("Cpf")))
# }

#' @importFrom dplyr %>% 
#' @importFrom rlang sym syms
read_sistec_web <- function(path){
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)
  
  vars <- c("NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA",
            "NO_STATUS_MATRICULA","NO_CURSO",
            "DT_DATA_INICIO", "CO_UNIDADE_ENSINO")
  
  co_unidade_ensino <- utils::read.csv(system.file("extdata/co_unidade_ensino/ifpe.csv",
                                                   package = "sistec"),
                                       colClasses = "character")
  
  classes <- c(NU_CPF = "character", CO_UNIDADE_ENSINO = "character")

  sistec <- lapply(temp, utils::read.csv,
         sep = ";",  stringsAsFactors = FALSE, 
         colClasses = classes, encoding = "latin1") %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(NU_CPF = num_para_cpf(
                    ifelse(stringr::str_length(!!sym("NU_CPF")) == 0, 
                           !!sym("NU_CPF"), # in API number zero in the beguinning is blank
                           stringr::str_pad(!!sym("NU_CPF"), 11, pad = "0")))) %>% 
    dplyr::mutate(DT_DATA_INICIO = stringr::str_remove(!!sym("DT_DATA_INICIO"),
                                                       " .*$")) %>% 
    dplyr::mutate(NO_CURSO = stringr::str_remove(!!sym("NO_CICLO_MATRICULA"), " - .*$")) %>% 
    dplyr::select(!!!syms(vars)) %>% 
    dplyr::left_join(co_unidade_ensino, by = "CO_UNIDADE_ENSINO") %>% 
    dplyr::select(-(!!sym("CO_UNIDADE_ENSINO")))
  
  class(sistec) <- c(class(sistec), "sistec_data_frame")
  sistec
}




#' @importFrom dplyr %>% 
#' @importFrom rlang sym
read_sistec_setec <- function(path){
  
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)
  
  classes <- c(Numero.Cpf = "character")
  
  sistec <- lapply(temp, utils::read.csv,
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
                     DT_DATA_INICIO = !!sym("Dt.Data.Inicio"),
                     NO_CAMPUS = stringr::str_sub(!!sym("Unidade.Ensino"), 42))
  
  class(sistec) <- c(class(sistec), "sistec_data_frame")
  sistec
}

# # será removido depois
# #' @importFrom dplyr %>% 
# #' @importFrom rlang sym syms
# read_sistec_simplified <- function(path){
#   
#   temp = list.files(path = path, pattern = "*.csv")
#   temp <- paste0(path, "/", temp)
#   
#   classes <- c(NU_CPF = "character", CO_CICLO_MATRICULA = "character",
#                NO_STATUS_MATRICULA = "character")
#   
#   sistec_vars <- c("NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA", "NO_STATUS_MATRICULA")
# 
#   lapply(temp, utils::read.csv,
#          sep = ";",  stringsAsFactors = FALSE, 
#          colClasses = classes, encoding = "latin1") %>%
#     dplyr::bind_rows() %>% 
#     dplyr::mutate(NU_CPF =num_para_cpf(!!sym("NU_CPF"))) %>% 
#     dplyr::select(!!!syms(sistec_vars))
# }

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
