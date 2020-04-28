#' Comparison between Sistec and a student registration dataset
#'
#' A generic function to compare and save the student situation. This function also 
#' shows inconsistencies in the datasets. You can pass the folder's file path or 
#' a data frame read by `sistec::read_*()` functions.
#' 
#' @param sistec The folder's path to Sistec files or the Sistec data frame
#' read by `sistec::read_sistec()`function.  
#' @param student_registration The folder's path to students reagistration datasets or a 
#' data frame read by `sistec::read_*()`functions. 
#' 
#' @return  A list of data frames. 
#' 
#' @examples 
#' # these datasets are not real. It is just for test purpose.
#' qacademico <- read_qacademico(system.file("extdata/examples/qacademico",
#'                                           package = "sistec"))
#' sistec <- read_sistec(system.file("extdata/examples/sistec",
#'                                   package = "sistec"))
#'                                   
#' compare_sistec(sistec, qacademico)                                   
#' 
#' @export
compare_sistec <- function(sistec, student_registration){
  
  UseMethod("compare_sistec", student_registration)
}

#' @export
compare_sistec.qacademico_data_frame <- function(sistec, student_registration){

  compare_sistec_qacademico(sistec, student_registration)
}

#' @export
compare_sistec.character <- function(sistec, student_registration){

  # We only have qacademico yet. This function will be modified when we 
  # have other registration system

  sistec <- sistec::read_sistec(sistec)
  qacademico <- sistec::read_qacademico(student_registration)
  compare_sistec_qacademico(sistec, qacademico)
}
