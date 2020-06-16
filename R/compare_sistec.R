#' Comparison between Sistec and a student registration dataset
#'
#' A generic function to compare and save the student situation. This function also 
#' shows inconsistencies in the datasets. You can pass the folder's file path or 
#' a data frame read by `sistec::read_*()` functions.
#' 
#' @param sistec The folder's path to Sistec files or the Sistec data frame
#' read by `sistec::read_sistec()`function.  
#' @param rfept The folder's path to students reagistration datasets or a 
#' data frame read by `sistec::read_*()`functions. 
#' 
#' @return  A list of data frames. 
#' 
#' @examples 
#' # these datasets are not real. It is just for test purpose.
#' 
#' sistec <- read_sistec(system.file("extdata/examples/sistec",
#'                                   package = "sistec"))
#'                                   
#' qacademico <- read_qacademico(system.file("extdata/examples/qacademico",
#'                                           package = "sistec"))
#'                                           
#' compare_sistec(sistec, qacademico)                                   
#' 
#' @export
compare_sistec <- function(sistec, rfept){
  UseMethod("compare_sistec", rfept)
}

#' @export
compare_sistec.rfept_data_frame <- function(sistec, rfept){
  compare_sistec_rfept(sistec, rfept)
}
