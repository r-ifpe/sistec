#' Comparison between Sistec and a student registration dataset
#'
#' A generic function to compare and save the student situation. This function also 
#' shows inconsistecies in the datasets. You can pass the folder's file path or 
#' a data frame read by `sistec::read_*()` functions.
#' 
#' @param sistec The folder's path to Sistec files or the Sistec data frame
#' read by `sistec::read_sistec()`function.  
#' @param student_registration The folder's path to students reagistration datasets or a 
#' data frame read by `sistec::read_*()`functions. 
#' @param output_path Specify a path if you want to save the comparison. The results will 
#' be saved in a folder named by `output_folder_name` parameter. The files created will be 
#' saved in xlsx format.
#' @param output_folder_name The folder name where the comparison will be saved. If not 
#' specified, the folder name will be "Sistec". This parameter is useless if `output_path` 
#' was not specified.
#' 
#' @export
compare_sistec <- function(sistec, student_registration,
                           output_path = NULL,
                           output_folder_name = "Sistec"){
  
  UseMethod("compare_sistec", student_registration)
}

#' @export
compare_sistec.qacademico_data_frame <- function(sistec, student_registration,
                                                 output_path = NULL,
                                                 output_folder_name = "Sistec"){

  compare_sistec_qacademico(sistec, student_registration,
                            output_path, output_folder_name)
}

#' @export
compare_sistec.character <- function(sistec, student_registration,
                                     output_path = NULL,
                                     output_folder_name = "Sistec"){

  # We only have qacademico yet. This function will be modified when we 
  # have other registration system

  sistec <- sistec::read_sistec(sistec)
  qacademico <- sistec::read_qacademico(student_registration)
  compare_sistec_qacademico(sistec, qacademico,
                            output_path, output_folder_name)
}
