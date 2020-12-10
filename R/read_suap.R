#' Read Suap files
#'
#' This function supports two kinds of schemas: from the api and the website. See Details 
#' if you need help to download the Suap data.
#'
#' @param path The Suap file's path. 
#' @param start A character with the date to start the comparison. The default is the minimum 
#' value found in the data. The date has to be in this format: "yyyy.semester".
#' Ex.: "2019.1" or "2019.2".
#' @return A data frame.
#' 
#' @importFrom dplyr %>% sym 
#' @export
read_suap <- function(path = "", start = NULL){
  
  if(path == "") stop("You need to specify the path.")
  
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  suap <- utils::read.csv(temp[1], sep = "\t",  stringsAsFactors = FALSE, 
                          encoding = "latin1", skip= 1, nrows = 1,
                          check.names = FALSE)
  
  vars <- c("Matr\u00edcula", "Nome", "Campus", "Data de Matr\u00edcula",
            "Descri\u00e7\u00e3o do Curso", "Situa\u00e7\u00e3o no Curso", "CPF")
            
  if(sum(names(suap) %in% vars) == 7){
    suap <- read_suap_web(path)
  } else{
    stop(paste("Not found:",
               paste(vars[!vars %in% names(suap)], collapse = ", ")))
  }
  
  suap <- filter_rfept_date(suap, start)
  suap
}

#' @importFrom dplyr %>% sym
read_suap_web <- function(path){
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  classes <- "character"
  
  suap <- lapply(temp,  utils::read.csv, skip = 1,
                       sep = "\t",  stringsAsFactors = FALSE, colClasses = classes,
                       encoding = "latin1", check.names = FALSE) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct(!!sym("Matr\u00edcula"), .keep_all = TRUE) %>% # Take the most recent 
    dplyr::transmute(R_NO_ALUNO = !!sym("Nome"),
                     R_NU_CPF = num_para_cpf(!!sym("CPF")),
                     R_CO_MATRICULA = !!sym("Matr\u00edcula"),
                     R_CO_CICLO_MATRICULA = "", # unitl now a RFEPT doesn't have ciclo
                     R_NO_STATUS_MATRICULA = !!sym("Situa\u00e7\u00e3o no Curso"),
                     R_NO_CURSO = suap_course_name(!!sym("Descri\u00e7\u00e3o do Curso")),
                     R_DT_INICIO_CURSO = suap_convert_beginning_date(!!sym("Data de Matr\u00edcula")),
                     R_NO_CAMPUS = suap_correct_campus_name(!!sym("Campus")),
                     R_NO_COTA = suap_cota(!!sym("Forma de Ingresso")))
  
  class(suap) <- c("rfept_data_frame", "suap_table", class(suap))
  
  suap
}

suap_convert_beginning_date <- function(date){
  
  year <- stringr::str_sub(date, 7, 10)
  month <- as.numeric(stringr::str_sub(date, 4, 5))
  
  year_semester <- paste0(year, ".",
                          ifelse(month <= 6, 1, 2))
  ifelse(year_semester == "NA.NA", "SEM DATA", year_semester)
}


suap_correct_campus_name <- function(campus){
  dplyr::if_else(campus == "", "SEM CAMPUS", campus)
}

suap_course_name <- function(course){ 
  course <- stringr::str_replace_all(course,"/|:|\\?|\\.", "_" )
  dplyr::if_else(course == "", "SEM CURSO", course)
}

suap_cota <- function(cota){
  dplyr::if_else(stringr::str_detect(cota, "Renda|Qualquer|Autodeclarados"),
                 "COTISTA", "N\u00c3O COTISTA")
}
