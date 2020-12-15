#' Comparison between Sistec and a student registration dataset
#'
#' A generic function to compare and save the student situation. This function also 
#' shows inconsistencies in the datasets. You can pass the folder's file path or 
#' a data frame read by `sistec::read_*()` functions. In most cases, there are no link
#' between courses in Sistec and the academic registration. You can pass this relationship
#' using `linked_courses` parameter or using ARIA estimation.
#' 
#' @param sistec The folder's path to Sistec files or the Sistec data frame
#' read by `read_sistec()`function.  
#' @param rfept The folder's path to students reagistration datasets or a 
#' data frame read by `sistec::read_*()`functions. 
#' @param linked_courses By default, the linked courses will be estimate using the data
#'  (ARIA estimation). You can specify those links loadind a .xlsx/csv file with linked courses 
#'  between the rfept and sistec. The columns must be in this order: INICIO, CICLO, 
#'  CURSO_SISTEC	CURSO_RFEPT	CAMPUS. The date in INICIO column must be in 
#'  yyyy.period. Ex.: 2019.1 or 2019.2. 
#' 
#' @return  A list of data frames. 
#' 
#' @examples 
#' # these datasets are not real. It is just for test purpose.
#' 
#' sistec <- read_sistec(system.file("extdata/examples/sistec",
#'                                   package = "sistec"))
#'                                   
#' rfept <- read_rfept(system.file("extdata/examples/qacademico",
#'                                 package = "sistec"))
#'                                           
#' compare_sistec(sistec, rfept) 
#' @export
compare_sistec <- function(sistec, rfept, linked_courses = NULL){
  UseMethod("compare_sistec", rfept)
}

#' @export
compare_sistec.rfept_data_frame <- function(sistec, rfept, linked_courses = NULL){
  compare_sistec_rfept(sistec, rfept, linked_courses)
}
