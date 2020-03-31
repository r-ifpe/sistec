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
read_sistec <- function(path = "extdata"){
  
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)

  # read just the first row to infer the schema
  sistec <- utils::read.csv(temp[1], sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

  # there are two kinds of sistec schema
  if (any(grepl("Numero.Cpf", names(sistec)))){
    sistec_api_schema(temp)
  } else {
    sistec_website_schema(temp)
  }
}

#' @rdname read_files
#' @importFrom rlang sym
#' @export
read_qacademico <- function(path = "extdata"){
  temp = list.files(path = path, pattern = "*.xlsx")
  temp <- paste0(path , "/", temp)
  lapply(temp, openxlsx::read.xlsx) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Cpf= num_para_cpf(!!sym("Cpf")))
}


server_input_path <- function(input_path){
  slash <- stringr::str_locate_all(input_path[1], "/")
  last_slash <- slash[[1]][nrow(slash[[1]]), 2]
  substr(input_path[1], 1, last_slash)
}

#' @importFrom dplyr %>% 
#' @importFrom rlang sym
sistec_api_schema <- function(temp){
  classes <- c(Numero.Cpf = "character")

  lapply(temp, utils::read.csv,
         sep = ";",  stringsAsFactors = FALSE, 
         colClasses = classes, encoding = "UTF-8") %>%
    dplyr::bind_rows() %>%
    dplyr::transmute(NO_ALUNO = !!sym("Nome.Aluno"), 
                     NU_CPF = num_para_cpf(!!sym("Numero.Cpf")),
                     CO_CICLO_MATRICULA = !!sym("Co.Ciclo.Matricula"), 
                     NO_STATUS_MATRICULA = !!sym("Situa\u00e7\u00e3o.Matricula"), # Situação.Matricula,
                     NO_CURSO = !!sym("No.Curso"), 
                     DT_DATA_INICIO = !!sym("Dt.Data.Inicio"))
}

#' @importFrom dplyr %>% 
#' @importFrom rlang sym syms
sistec_website_schema <- function(temp){
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
