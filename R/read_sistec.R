#' Read sistec files
#'
#' The package provides support if your data comes 
#' from [setec](http://portal.mec.gov.br/setec-secretaria-de-educacao-profissional-e-tecnologica)
#' or [web](https://sistec.mec.gov.br/). You just need to pass the folder's path were are your files.
#' See Details if you need help to download the data from Sistec. 
#'
#' @param path The sistec file's path. 
#' @param start A character with the date to start the comparison. The default is the minimum 
#' value found in the data. The date has to be in this format: "yyyy.semester".
#' Ex.: "2019.1" or "2019.2".
#' @return A data frame.
#' 
#' @details You can download the Sistec's student registration using your proper account on 
#' Sistec.  Be sure that your data has these variables: 
#'   
#'  - On setec: "Nome Aluno", "Numero Cpf", "Co Ciclo Matricula", "Situacao Matricula",
#'   "No Curso", "Dt Data Inicio" and "Unidade Ensino".
#'  - On web: "NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA", "NO_STATUS_MATRICULA", "NO_CICLO_MATRICULA",
#'  "DT_DATA_INICIO" and "CO_UNIDADE_ENSINO".
#'
#' Tip: To take every student for your institution/campus using web, search by student name and use " ".
#' 
#' @examples  
#' # this dataset is not a real one. It is just for test purpose.
#' sistec <- read_sistec(system.file("extdata/examples/sistec",
#'                                   package = "sistec"))
#' 
#' sistec
#' 
#' # example selecting the period
#' sistec_2019_2 <- read_sistec(system.file("extdata/examples/sistec", package = "sistec"),
#'                                 start = "2019.2") 
#' 
#' sistec_2019_2 
#' @export
read_sistec <- function(path = "", start = NULL){
  
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
                            encoding = "UTF-8", check.names = FALSE, nrows = 100)
  
  num_vars_setec <- sum(names(sistec) %in% vars_setec)
  num_vars_web <- sum(names(sistec) %in% vars_web)
  
  if(num_vars_setec > 0){
    if(num_vars_setec < 7){ 
      stop(paste("Not found:",
                 paste(vars_setec[!vars_setec %in% names(sistec)], collapse = ", ")))
      
    } else{
      encoding <- sistec_web_encoding(sistec)
      sistec <- read_sistec_setec(path, encoding)
    }
  } else if(num_vars_web > 0){
    if(num_vars_web < 7){ 
      stop(paste("Not found:",
                 paste(vars_web[!vars_web %in% names(sistec)], collapse = ", ")))
      
    } else{
      encoding <- sistec_web_encoding(sistec)
      sistec <- read_sistec_web(path, encoding)
    }
  } else {
    stop("Not found Sistec variables in your file.")
  }
  
  sistec <- filter_sistec_date(sistec, start)
  sistec
}

#' @importFrom dplyr %>% sym
read_sistec_web <- function(path, encoding){
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)

  co_unidade_ensino <- co_unidade_ensino()
  classes <- c(NU_CPF = "character", CO_UNIDADE_ENSINO = "character")

  sistec <- lapply(temp, utils::read.csv,
                   sep = ";",  stringsAsFactors = FALSE, 
                   colClasses = classes, encoding = encoding) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(co_unidade_ensino, by = "CO_UNIDADE_ENSINO") %>% 
    dplyr::transmute(S_NO_ALUNO = !!sym("NO_ALUNO"),
                     S_NU_CPF = num_para_cpf(!!sym("NU_CPF")),
                     S_CO_CICLO_MATRICULA = !!sym("CO_CICLO_MATRICULA"),
                     S_NO_STATUS_MATRICULA = !!sym("NO_STATUS_MATRICULA"),
                     S_NO_CURSO = sistec_course_name(!!sym("NO_CICLO_MATRICULA")),
                     S_DT_INICIO_CURSO = sistec_convert_beginning_date(!!sym("DT_DATA_INICIO"), encoding),
                     S_NO_CAMPUS = !!sym("S_NO_CAMPUS"))
  
  class(sistec) <- c("sistec_data_frame", class(sistec))
  sistec
}

#' @importFrom dplyr %>% sym
read_sistec_setec <- function(path, encoding){
  
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)
  
  classes <- c(Numero.Cpf = "character")

  sistec <- lapply(temp, utils::read.csv,
                   sep = ";",  stringsAsFactors = FALSE, 
                   colClasses = classes, encoding = "UTF-8") %>%
    dplyr::bind_rows() %>%
    dplyr::transmute(S_NO_ALUNO = !!sym("Nome.Aluno"), 
                     S_NU_CPF = num_para_cpf(!!sym("Numero.Cpf")),
                     S_CO_CICLO_MATRICULA = !!sym("Co.Ciclo.Matricula"), 
                     S_NO_STATUS_MATRICULA = !!sym("Situa\u00e7\u00e3o.Matricula"), # Situação.Matricula
                     S_NO_CURSO = sistec_course_name(!!sym("No.Curso")), 
                     S_DT_INICIO_CURSO = sistec_convert_beginning_date(!!sym("Dt.Data.Inicio"), encoding),
                     S_NO_CAMPUS = sistec_campus_name(!!sym("Unidade.Ensino")))

  class(sistec) <- c("sistec_data_frame", class(sistec))
  sistec
}

sistec_convert_beginning_date <- function(date, encoding){
  
  if (encoding == "latin1"){
    year <- stringr::str_sub(date, 7, 10)
    month <- as.numeric(stringr::str_sub(date, 4, 5))
    semester <- ifelse(month > 6, 2, 1)
    paste0(year, ".", semester )
  } else{
    year <- stringr::str_sub(date, 1, 4)
    month <- as.numeric(stringr::str_sub(date, 5, 6))
    semester <- ifelse(month > 6, 2, 1)
    paste0(year, ".", semester )
  }
}

sistec_course_name <- function(course){
  course <- stringr::str_remove(course, " - .*$")  
  course <- stringr::str_replace_all(course,"/|:|\\?|\\.", "_" )
}

sistec_campus_name <- function(campus){
  campus <- stringr::str_sub(campus, 42)
  sistec_correct_campus_name(campus)
}

sistec_correct_campus_name <- function(campus){
  stringr::str_replace(campus, "REU E LIMA", "ABREU E LIMA")
}

sistec_web_encoding <- function(x){
  utf <- any(stringr::str_detect(x$NO_STATUS_MATRICULA,
                                 "CONCLU\u00cdDA"))
  encoding <- if(utf){
    "UTF-8"
  } else{
    "latin1"
  }
  encoding
}

filter_sistec_date <- function(x, start){
  
  year_regex <- "[12][09][0-9]{2}.[12]"
  
  if(is.null(start)){
    start <- stringr::str_extract(x$S_DT_INICIO_CURSO, year_regex) %>% 
      min(na.rm = TRUE)
  } 
  
  x %>% 
    dplyr::filter(!!sym("S_DT_INICIO_CURSO") >= start)
}