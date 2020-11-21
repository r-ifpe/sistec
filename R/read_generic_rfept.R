#' Read generic rfept files
#'
#' If your institute is not fully integrated with the package, you can transform your 
#' academic database into a generic layout and use `read_rfept()` normally. See 
#' Details to understand the requisites.
#'
#' @param path The rfept file's path. 
#' @param start A character with the date to start the comparison. The default is the minimum 
#' value found in the data. The date has to be in this format: "yyyy.semester".
#' Ex.: "2019.1" or "2019.2".
#' @return A data frame.
#' 
#' @details To transform your database in a generic rfept layout, follow these 
#' requirements: 
#'  
#' - Rename your columns to: NO_ALUNO, NU_CPF, CO_MATRICULA, NO_STATUS_MATRICULA,
#' NO_CURSO, DT_INICIO_CURSO, NO_CAMPUS and NO_COTA;
#' - All variables should be inherited to string class;
#' - The CPF's should be in xxx.xxx.xxx-xx format;
#' - The beginning date should be in yyyy.s format. Ex.: 2020.1. Use 1 for first 
#' semester and 2 for second.
#' - Convert the student's status to valid name in Sistec, use: ABANDONO, EM_CURSO,
#' CONCLUÍDA, DESLIGADO, INTEGRALIZADA, REPROVADA and TRANSF_EXT;
#' - Save your data in a single file in csv format separated by comma and use 
#' latin1 encoding. Semicolons separators and UTF-8 enconding are also available.   
#' 
#' @examples  
#' # this dataset is not a real one. It is just for test purpose.
#' rfept <- read_generic_rfept(system.file("extdata/examples/generic_rfept",
#'                                         package = "sistec"))
#' 
#' rfept
#' @importFrom dplyr %>% 
#' @export
read_generic_rfept <- function(path = "", start = NULL){
  
  if(path == "") stop("You need to specify the path.")
  
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  if(length(temp) > 1) stop("Somente um arquivo pode ser carregado.")
  check_rfept_header(temp)
  sep <- detect_sep(temp)
  encoding <- detect_encoding(temp, sep)
  
  rfept <- utils::read.csv(temp, sep = sep,  stringsAsFactors = FALSE, 
                           colClasses = "character",
                           encoding = encoding, check.names = FALSE) %>% 
    dplyr::distinct() %>% 
    dplyr::transmute(R_NO_ALUNO = !!sym("NO_ALUNO"),
                     R_NU_CPF = !!sym("NU_CPF"),
                     R_CO_MATRICULA = !!sym("CO_MATRICULA"),
                     R_CO_CICLO_MATRICULA = "", # unitl now a RFEPT doesn't have ciclo
                     R_NO_STATUS_MATRICULA = !!sym("NO_STATUS_MATRICULA"),
                     R_NO_CURSO = correct_course_name(!!sym("NO_CURSO")),
                     R_DT_INICIO_CURSO = !!sym("DT_INICIO_CURSO"),
                     R_NO_CAMPUS = !!sym("NO_CAMPUS"),
                     R_NO_COTA = !!sym("NO_COTA"))

  check_rfept_variable_class(rfept)
  check_rfept_cpf(rfept)
  check_rfept_course_beginning_date(rfept)
  check_rfept_status(rfept)
  
  class(rfept) <- c("rfept_data_frame", "generic_rfept_table", class(rfept))
  
  rfept
}

check_rfept_header <- function(x){
  header <- readLines(x, n = 1, encoding = "UTF-8")
  columns <- c(stringr::str_detect(header, "NO_ALUNO"),
               stringr::str_detect(header, "NU_CPF"),
               stringr::str_detect(header, "CO_MATRICULA"),
               stringr::str_detect(header, "NO_STATUS_MATRICULA"),
               stringr::str_detect(header, "DT_INICIO_CURSO"),
               stringr::str_detect(header, "NO_CAMPUS"),
               stringr::str_detect(header, "NO_COTA"))
  
  if(sum(columns) != 7){
    rfept_header <- c("NO_ALUNO", "NU_CPF", "CO_MATRICULA", "NO_STATUS_MATRICULA",
                      "NO_CURSO", "DT_INICIO_CURSO", "NO_CAMPUS", "NO_COTA")
    
    if(sum(columns) == 1) {
      stop(paste0("Sistema acad\u00eamico: A coluna ",
                  rfept_header[!columns], " est\u00e1 faltando."),
           call. = FALSE)
    } else {
      stop(paste0("Sistema acad\u00eamico: As colunas ",
                  paste0(rfept_header[!columns], collapse = ", "),
                  " est\u00e3o faltando."),
           call. = FALSE)
    }
  }
}

check_rfept_variable_class <- function(x){
  if(!all(unlist(lapply(x, class)) == "character")){
    stop("Sistema acad\u00eamico: Todas as classes precisam ser do tipo texto (string).",
         call. = FALSE)
  } 
}

check_rfept_cpf <- function(x){
  all_true <- all(grepl("([0-9]{3}[.][0-9]{3}[.][0-9]{3}[-][0-9]{2})", x$R_NU_CPF))
  if(!all_true){
    stop(paste("Sistema acad\u00eamico: Os CPF's n\u00e3o est\u00e3o no formato",
               "xxx.xxx.xxx-xx."), call. = FALSE)
  }
}

check_rfept_course_beginning_date <- function(x){
  all_true <- all(grepl("([19|20][0-9]{2}[.][12])", x$R_DT_INICIO_CURSO))
  if(!all_true){
    stop(paste("Sistema acad\u00eamico: A data de in\u00edcio n\u00e3o est\u00e1",
               "no formato aaaa.s. Ex.: 2020.1.",
               "Verifique tamb\u00e9m se o per\u00edodo est\u00e1 correto: 1 para",
               "o primeiro semestre e 2 para o segundo."), call. = FALSE)
  }
}

check_rfept_status <- function(x){
 all_true <- all(x$R_NO_STATUS_MATRICULA %in% c("ABANDONO", "EM_CURSO",
      "CONCLU\u00cdDA", "DESLIGADO", "INTEGRALIZADA", "REPROVADA", "TRANSF_EXT"))
 
 if(!all_true){
   stop(paste("Sistema acad\u00eamico: Os status dos alunos n\u00e3o est\u00e3o",
              "no formato do Sistec.",
              "Utilize: ABANDONO, CONCLU\u00cdDA, DESLIGADO, EM_CURSO,",
              "INTEGRALIZADA, REPROVADA ou TRANSF_EXT"),
        call. = FALSE)
 }
}

correct_course_name <- function(course){
  course <- stringr::str_remove(course, " - .*$")  
  course <- stringr::str_replace_all(course,"\\\\|\"|/|:|\\?|\\.", "_" )
}
