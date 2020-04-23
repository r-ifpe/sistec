#' Read sistec files
#'
#' This function support two kinds of schemas: from the api (setec) and from the website.
#'
#' @param path The sistec file's path. 
#' @return A data frame 
#' @examples 
#' 
#' # this dataset is not a real one. It is just for test purpose.
#' sistec <- read_sistec(system.file("extdata/examples/sistec",
#'                                   package = "sistec"))
#' 
#' sistec
#' 
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
             !!sym("NU_CPF"), # in API, number zero in the beginning is blank
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
                     NO_CAMPUS = stringr::str_sub(!!sym("Unidade.Ensino"), 42)) %>% 
    dplyr::mutate(NO_CAMPUS = ifelse(!!sym("NO_CAMPUS") == "REU E LIMA",  # Register name is wrong  
                                     "ABREU E LIMA", !!sym("NO_CAMPUS"))) # in setec
  
  class(sistec) <- c(class(sistec), "sistec_data_frame")
  sistec
}