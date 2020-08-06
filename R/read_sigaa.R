#' Read Sigaa files
#'
#' This function reads partial and complete Sigaa datasets. These two formats can perform the
#' comparison, but the partial doesn't have information about "Campus" and "Cota". See Details
#' if you need help where to download the Sigaa data.
#'
#' @param path The Sigaa file's path. 
#' @param start A character with the date to start the comparison. The default is the minimum 
#' value found in the data. The date has to be in this format: "yyyy.semester".
#' Ex.: "2019.1" or "2019.2".
#' @return A data frame.
#' 
#' @details To download the partial Sigaa's data, go to your proper account on Sigaa and 
#' follow:
#'  
#' - Access the panel "Consultas" inside Sigaa module.
#' - Generate the report "Consulta geral discentes".
#' - Select the check box "Trazer informações em forma de relatório" e "Gerar csv".
#' - Select the filter "Campus" and other filters you desire.
#' - Click on "Buscar" and download the file. 
#'
#' Be sure that your data has the variables: "Matricula", "Nome", "Status, 
#' "Curso" and "CPF". 
#' 
#' For the complete dataset, you have to download directly from the Sigaa database. Be sure 
#' that your data has the variables: "Matricula", "Nome", "Situacao Matricula", "Curso",
#' "Cpf", "Instituicao", "ano_ingresso", "semestre_ingresso" and "Cota".
#' @examples  
#' # this dataset is not a real one. It is just for test purpose.
#' sigaa <- read_sigaa(system.file("extdata/examples/sigaa",
#'                                 package = "sistec"))
#' 
#' sigaa
#' @importFrom dplyr %>% sym 
#' @export
read_sigaa <- function(path = "", start = NULL){

  if(path == "") stop("You need to specify the path.")
  
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  sigaa <- utils::read.csv(temp[1], sep = ";",  stringsAsFactors = FALSE, 
                                encoding = "UTF-8", nrows = 1, check.names = FALSE)

  vars_complete <- c("Matricula", "Nome", "Situacao Matricula", "Curso", "Cpf",
                     "Instituicao", "ano_ingresso", "semestre_ingresso", "Cota")  
  vars_partial <- c("Matr\u00edcula", "Nome", "Status", "Curso", "CPF")
  
  sigaa_complete <- any(grepl("Situacao Matricula", names(sigaa)))
  sigaa_partial <- any(grepl("Status", names(sigaa)))
 
  num_vars_complete <- sum(names(sigaa) %in% vars_complete)
  num_vars_partial <- sum(names(sigaa) %in% vars_partial)
  
  if(sigaa_complete){
    if(num_vars_complete == 9){ 
      sigaa <- read_sigaa_complete(path)
    } else{
      stop(paste("Not found:",
                 paste(vars_complete[!vars_complete %in% names(sigaa)], collapse = ", ")))
    }
  } else if(sigaa_partial){
    if(num_vars_partial == 5){ 
      sigaa <- read_sigaa_partial(path)
    } else{
      stop(paste("Not found:",
                 paste(vars_partial[!vars_partial %in% names(sigaa)], collapse = ", ")))
    }
  } else {
    stop("Not found Sigaa variables in your file.")
  }

  sigaa <- filter_rfept_date(sigaa, start)
  sigaa
}

#' @importFrom dplyr %>% sym
read_sigaa_complete <- function(path){
  
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  classes <- c(Cpf = "character")
  
  sigaa <- lapply(temp,  utils::read.csv,
                  sep = ";",  stringsAsFactors = FALSE, colClasses = classes,
                  encoding = "UTF-8", check.names = FALSE) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct(!!sym("Matricula"), .keep_all = TRUE) %>% # Take the most recent 
    dplyr::transmute(R_NO_ALUNO = !!sym("Nome"),
                     R_NU_CPF = num_para_cpf(!!sym("Cpf")),
                     R_CO_MATRICULA = !!sym("Matricula"),
                     R_CO_CICLO_MATRICULA = "", # unitl now a RFEPT doesn't have ciclo
                     R_NO_STATUS_MATRICULA = !!sym("Situacao Matricula"),
                     R_NO_CURSO = sigaa_course_name(!!sym("Curso")),
                     R_DT_INICIO_CURSO = sigaa_complete_convert_beginning_date(!!sym("ano_ingresso"), !!sym("semestre_ingresso")),
                     R_NO_CAMPUS = sigaa_campus_name(!!sym("Instituicao")),
                     R_NO_COTA = sigaa_cota(!!sym("Cota")))
  
  class(sigaa) <- c("rfept_data_frame", "sigaa_table", class(sigaa))
  
  sigaa
}

sigaa_campus_name <- function(campus){
  campus <- stringr::str_sub(campus, 8)
  sigaa_correct_campus_name(campus)
}

sigaa_complete_convert_beginning_date <- function(year, semester){
  paste0(year, ".", semester)
}


sigaa_cota <- function(cota){
  dplyr::if_else(grepl("Processo seletivo sem cota|Ampla concorr\u00eancia", cota), 
                 "N\u00c3O COTISTA",
                 dplyr::if_else(cota == "",
                                "SEM INFORMA\u00c7\u00c3O",
                                "COTISTA"))
}

#' @importFrom dplyr %>% sym
read_sigaa_partial <- function(path){
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
                     R_DT_INICIO_CURSO = sigaa_partial_convert_beginning_date(!!sym("Matr\u00edcula")),
                     R_NO_CAMPUS = "SEM CAMPUS", # until now SIGAA doesn't have campus information
                     R_NO_COTA = "SEM INFORMA\u00c7\u00c3O")
  
  class(sigaa) <- c("rfept_data_frame", "sigaa_table", class(sigaa))
  
  sigaa
}

sigaa_partial_convert_beginning_date <- function(mat){
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
