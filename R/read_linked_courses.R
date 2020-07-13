#' Read linked courses files
#'
#' This function reads a .xlsx file with the linked courses between Sistec and the academic 
#' registration.  The columns must be in this order: INICIO, CICLO, CURSO_SISTEC, 
#' CURSO_RFEPT and CAMPUS. The date in INICIO column must be in yyyy.period. 
#' Ex.: 2019.1 or 2019.2. .
#'
#' @param path The linked courses file's path. 
#' 
#' @return A data frame.
#' 
#' @examples  
#' linked_courses <- read_linked_courses(system.file("extdata/examples/linked_courses",
#'                                                   package = "sistec"))
#' 
#' linked_courses
#' @export
read_linked_courses <- function(path = ""){
  
  if(path == "") stop("You need to specify the path.")
  
  temp = list.files(path = path, pattern = "*.xlsx")
  temp <- paste0(path, "/", temp)
  
  openxlsx::read.xlsx(temp)
}
