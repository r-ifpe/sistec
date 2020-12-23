#' Read Conecta files
#'
#' This function supports two kinds of schemas: from the api and the website. See Details 
#' if you need help to download the Conecta data.
#'
#' @param path The Conecta file's path. 
#' @param start A character with the date to start the comparison. The default is the minimum 
#' value found in the data. The date has to be in this format: "yyyy.semester".
#' Ex.: "2019.1" or "2019.2".
#' @return A data frame.
#' 
#' @importFrom dplyr %>% sym 
#' @export
read_conecta <- function(path = "", start = NULL){
  
  if(path == "") stop("You need to specify the path.")
  
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  conecta <- utils::read.csv(temp[1], sep = ";",  stringsAsFactors = FALSE, 
                             encoding = "latin1", nrows = 1, check.names = FALSE)
  
  
  vars <- c("RA", "NOME_ALUNO", "STATUS_NO_CURSO", "Cota Chamado",
            "NOME_CURSO", "CPF", "NOME_CAMPUS", "DATA_INGRESSO_CURSO")
  
  if(sum(names(conecta) %in% vars) == 8){
    conecta <- read_conecta_web(path)
  } else{
    stop(paste("Not found:",
               paste(vars[!vars %in% names(conecta)], collapse = ", ")))
  }
  
  conecta <- filter_rfept_date(conecta, start)
  conecta
}

#' @importFrom dplyr %>% sym
read_conecta_web <- function(path){
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  classes <- "character"
  
  conecta <- lapply(temp,  utils::read.csv,
                       sep = ";",  stringsAsFactors = FALSE, colClasses = classes,
                       encoding = "latin1", check.names = FALSE) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct(!!sym("RA"), .keep_all = TRUE) %>% # Take the most recent 
    dplyr::transmute(R_NO_ALUNO = !!sym("NOME_ALUNO"),
                     R_NU_CPF = num_para_cpf(!!sym("CPF")),
                     R_CO_MATRICULA = !!sym("RA"),
                     R_CO_CICLO_MATRICULA = "", # unitl now a RFEPT doesn't have ciclo
                     R_NO_STATUS_MATRICULA = !!sym("STATUS_NO_CURSO"),
                     R_NO_CURSO = correct_course_name(!!sym("NOME_CURSO")),
                     R_DT_INICIO_CURSO = conecta_convert_beginning_date(!!sym("DATA_INGRESSO_CURSO")),
                     R_NO_CAMPUS = !!sym("NOME_CAMPUS"),
                     R_NO_COTA = conecta_cota(!!sym("Cota Chamado")))
  
  class(conecta) <- c("rfept_data_frame", "conecta_table", class(conecta))
  
  conecta
}

## This will be deprecated
# conecta_convert_course_name <- function(course){
#   stringr::str_trim(course)
# }

conecta_convert_beginning_date <- function(date){
  
  year <- stringr::str_sub(date, 7, 10)
  month <- as.numeric(stringr::str_sub(date, 4, 5))
  
  year_semester <- paste0(year, ".",
                          ifelse(month <= 6, 1, 2))
  ifelse(year_semester == "NA.NA", "SEM DATA", year_semester)
}

conecta_cota <- function(cota){
  dplyr::if_else(grepl("A0|AC", cota) | cota == "",
                 "N\u00c3O COTISTA",
                 "COTISTA")
}