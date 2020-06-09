#' Read Sigaa files
#'
#' This function support two kinds of schemas: from the api and the website. See Details 
#' if you need help to download the Qacademico data.
#'
#' @param path The Sigaa file's path. 
#' @return A data frame.
#' 
#' @details  To download the student's data, go to your proper account on Sigaa and 
#' follow:
#'  
#' - Access the panel "Consultas" inside Sigaa module.
#' - Generate the report "Consulta geral discentes".
#' - Select the check box "Trazer informações em forma de relatório" e "Gerar csv".
#' - Select the filter "Campus" and other filter you desire.
#' - Selecionar o filtro "campus" ou outros filtros desejados.
#' - Click on "Buscar" and download the file. 
#'
#' Be sure that your data has the variables: "Matricula", "Nome", "Status, 
#' "Curso" and "CPF". 
#' 
#' @examples  
#' # this dataset is not a real one. It is just for test purpose.
#' sigaa <- read_sigaa(system.file("extdata/examples/sigaa",
#'                                 package = "sistec"))
#' 
#' sigaa
#' 
#' @importFrom dplyr %>% sym 
#' @export
read_sigaa <- function(path = ""){
  
  if(path == "") stop("You need to specify the path.")
  
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  sigaa <- utils::read.csv(temp[1], sep = ";",  stringsAsFactors = FALSE, 
                                encoding = "UTF-8", nrows = 1, check.names = FALSE)
  
  vars <- c("Matr\u00edcula", "Nome", "Status", "Curso", "CPF")
  
  if(sum(names(sigaa) %in% vars) == 5){
    read_sigaa_web(path)
  } else{
    stop(paste("Not found:",
               paste(vars[!vars %in% names(sigaa)], collapse = ", ")))
  }
}

#' @importFrom dplyr %>% sym
read_sigaa_web <- function(path){
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  classes <- c(CPF = "character")

  sigaa <- lapply(temp,  utils::read.csv,
                  sep = ";",  stringsAsFactors = FALSE, colClasses = classes,
                  encoding = "UTF-8", check.names = FALSE) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct(!!sym("Matr\u00edcula"), .keep_all = TRUE) %>% # Take the most recent 
    dplyr::transmute(R_NO_ALUNO = !!sym("Nome"),
                     R_NU_CPF = num_para_cpf(!!sym("CPF")),
                     R_CO_MATRICULA = !!sym("Matr\u00edcula"),
                     R_CO_CICLO_MATRICULA = "", # unitl now a RFEPT doesn't have ciclo
                     R_NO_STATUS_MATRICULA = !!sym("Status"),
                     R_NO_CURSO = sigaa_course_name(!!sym("Curso")),
                     R_DT_INICIO_CURSO = sigaa_convert_beginning_date(!!sym("Matr\u00edcula")),
                     R_NO_CAMPUS = "SEM CAMPUS", # until now SIGAA doesn't have campus information
                     R_NO_COTA = "SEM INFORMA\u00c7\u00c3O")
  
  class(sigaa) <- c("rfept_data_frame", "sigaa_table", class(sigaa))
  
  sigaa
}

sigaa_convert_beginning_date <- function(mat){
  len = stringr::str_length(mat)
  
  dplyr::case_when(
    len == 12 ~ paste0(stringr::str_sub(mat, 1, 4), ".", 
                       stringr::str_sub(mat, 5, 5)),
    len == 10 ~ ifelse(stringr::str_sub(mat, 1, 3) == 200,
                       paste0(stringr::str_sub(mat, 1, 4), ".",
                              stringr::str_sub(mat, 5,5)),
                       paste0("20",
                              stringr::str_sub(mat, 1, 2), ".", 
                              stringr::str_sub(mat, 3,3))),
    len == 9 ~ ifelse(stringr::str_sub(mat, 2, 2) == 0,
                      paste0(stringr::str_sub(mat, 1, 4), ".",
                             stringr::str_sub(mat, 5,5)),
                      paste0("200",
                             stringr::str_sub(mat, 1, 1), ".",
                             stringr::str_sub(mat, 2, 2))),
    len == 8 ~ paste0("19",
                      stringr::str_sub(mat, 1, 2), ".", 
                      stringr::str_sub(mat, 3,3))
  )
}

sigaa_correct_campus_name <- function(campus){
  dplyr::if_else(campus == "", "SEM CAMPUS", campus)
}

sigaa_course_name <- function(course){ 
  course <- stringr::str_replace_all(course,"/|:|\\?|\\.", "_" )
  dplyr::if_else(course == "", "SEM CURSO", course)
}
