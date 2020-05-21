#' Read Qacademico files
#'
#' This function support two kinds of schemas: from the api and the website. See Details 
#' if you need help to download the Qacademico data.
#'
#' @param path The Qacademico file's path. 
#' @return A data frame.
#' 
#' @details To download the student's data, go to your proper account on Qacademico and 
#' follow:
#'  
#' - "Relatorio de Alunos" -> "Listagem de Alunos" (choose year and period)
#' - Click on "visualizar" 
#' - Using F10 shortcut and save in .csv format
#' - Rename the including year and period (example2020_1.csv) 
#'
#' Be sure that your data has the variables: "Matricula", "Nome", "Situacao Matricula", 
#' "Curso", "Cpf", "Instituicao", "Per. Letivo Inicial".
#' 
#' @examples  
#' # this dataset is not a real one. It is just for test purpose.
#' qacademico <- read_qacademico(system.file("extdata/examples/qacademico",
#'                                           package = "sistec"))
#' 
#' qacademico
#' @importFrom rlang sym
#' @importFrom dplyr %>% 
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

#' @importFrom rlang sym
#' @importFrom dplyr %>% 
read_qacademico_web <- function(path){
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  classes <- c(Cpf = "character")

  qacademico <- lapply(temp,  utils::read.csv,
                sep = "",  stringsAsFactors = FALSE, 
                colClasses = classes, encoding = "latin1") %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct(!!sym("Matr\u00edcula"), .keep_all = TRUE) %>% # Take the most recent 
    dplyr::transmute(R_NO_ALUNO = !!sym("Nome"),
                     R_NU_CPF = num_para_cpf(!!sym("Cpf")),
                     R_CO_CICLO_MATRICULA = "", # unitl now a RFEPT doesn't have ciclo
                     R_NO_STATUS_MATRICULA = !!sym("Situa\u00e7\u00e3o.Matr\u00edcula"),
                     R_NO_CURSO = !!sym("Curso"),
                     R_DT_INICIO_CURSO = qacademico_convert_beginning_date(!!sym("Per..Letivo.Inicial")),
                     R_NO_CAMPUS = qacademico_campus_name(!!sym("Institui\u00e7\u00e3o")))
  
  class(qacademico) <- c("rfept_data_frame", class(qacademico))

  qacademico
}

qacademico_convert_beginning_date <- function(date){
  stringr::str_replace(date, "/", ".")
}

qacademico_campus_name <- function(campus){
  campus <- stringr::str_sub(campus,8)
  qacademico_correct_campus_name(campus)
}

qacademico_correct_campus_name <- function(campus){
  dplyr::if_else(campus == "", "SEM CAMPUS", campus)
}
