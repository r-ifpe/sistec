#' Read Qacademico files
#'
#' This function support two kinds of schemas: from the api and the website. See Details 
#' if you need help to download the Qacademico data.
#'
#' @param path The Qacademico file's path. 
#' @param start A character with the date to start the comparison. The default is the minimum 
#' value found in the data. The date has to be in this format: "yyyy.semester".
#' Ex.: "2019.1" or "2019.2".
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
#' "Curso", "Cpf", "Instituicao", "Per. Letivo Inicial" and "Cota".
#' 
#' @examples  
#' # this dataset is not a real one. It is just for test purpose.
#' qacademico <- read_qacademico(system.file("extdata/examples/qacademico",
#'                                           package = "sistec"))
#' 
#' qacademico
#' @importFrom dplyr %>% sym 
#' @export
read_qacademico <- function(path = "", start = NULL){

  if(path == "") stop("You need to specify the path.")
  
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)

  qacademico <- utils::read.csv(temp[1], sep = "",  stringsAsFactors = FALSE, 
                                encoding = "latin1", nrows = 1, check.names = FALSE)
  
  vars <- c("Matr\u00edcula", "Nome", "Situa\u00e7\u00e3o Matr\u00edcula", "Cota",
            "Curso", "Cpf", "Institui\u00e7\u00e3o", "Per. Letivo Inicial")
  
  if(sum(names(qacademico) %in% vars) == 8){
    qacademico <- read_qacademico_web(path)
  } else{
    stop(paste("Not found:",
               paste(vars[!vars %in% names(qacademico)], collapse = ", ")))
  }
  
  qacademico <- filter_rfept_date(qacademico, start)
  qacademico
}

#' @importFrom dplyr %>% sym
read_qacademico_web <- function(path){
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  classes <- "character"

  qacademico <- lapply(temp,  utils::read.csv,
                sep = "",  stringsAsFactors = FALSE, colClasses = classes,
                encoding = "latin1", check.names = FALSE) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct(!!sym("Matr\u00edcula"), .keep_all = TRUE) %>% # Take the most recent 
    dplyr::transmute(R_NO_ALUNO = !!sym("Nome"),
                     R_NU_CPF = num_para_cpf(!!sym("Cpf")),
                     R_CO_MATRICULA = !!sym("Matr\u00edcula"),
                     R_CO_CICLO_MATRICULA = "", # unitl now a RFEPT doesn't have ciclo
                     R_NO_STATUS_MATRICULA = !!sym("Situa\u00e7\u00e3o Matr\u00edcula"),
                     R_NO_CURSO = qacademico_course_name(!!sym("Curso")),
                     R_DT_INICIO_CURSO = qacademico_convert_beginning_date(!!sym("Per. Letivo Inicial")),
                     R_NO_CAMPUS = qacademico_campus_name(!!sym("Institui\u00e7\u00e3o")),
                     R_NO_COTA = qacademico_cota(!!sym("Cota")))
  
  class(qacademico) <- c("rfept_data_frame", "qacademico_table", class(qacademico))

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

qacademico_cota <- function(cota){
  dplyr::if_else(grepl("N\u00e3o possui|Ampla Concorr\u00eancia$", cota), 
                 "N\u00c3O COTISTA",
                 dplyr::if_else(cota == "",
                                "SEM INFORMA\u00c7\u00c3O",
                                "COTISTA"))
}

qacademico_course_name <- function(course){ 
  course <- stringr::str_replace_all(course,"/|:|\\?|\\.", "_" )
  dplyr::if_else(course == "", "SEM CURSO", course)
}
