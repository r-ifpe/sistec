#' Read linked courses files
#'
#' This function reads a .xlsx/.csv file with the linked courses between Sistec and the academic 
#' registration.  The columns must be in this order: INICIO, CICLO, CURSO_SISTEC, 
#' CURSO_RFEPT and CAMPUS. The date in INICIO column must be in yyyy.period. 
#' Ex.: 2019.1 or 2019.2. .
#'
#' @param path The linked courses file's path.
#' @param format You can choose between xlsx or csv.  
#' 
#' @return A data frame.
#' 
#' @examples  
#' linked_courses <- read_linked_courses(system.file("extdata/examples/linked_courses",
#'                                                   package = "sistec"), "csv")
#' 
#' linked_courses
#' @export
read_linked_courses <- function(path = "", format = "xlsx"){
  
  if(path == "") stop("You need to specify the path.")
  
  pattern <- paste0("*.", format)
  
  temp = list.files(path = path, pattern = pattern)
  temp <- paste0(path, "/", temp)
  
  if(format == "xlsx"){
    openxlsx::read.xlsx(temp) 
  } else{
    utils::read.csv(temp, colClasses = c("INICIO" = "character",
                                         "CICLO" = "integer"))
  }
}
