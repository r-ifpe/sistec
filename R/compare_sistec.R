#' Comparison between sistec and a student registration dataset
#'
#' Compare and save the student situation and the inconsistecies in the datasets.
#' 
#' 
#' @param sistec A Sistec data frame read by `sistec::read_sistec()`function. 
#' @param x A data frame read by `sistec::read_*()`functions. 
#' @param output_path Specify a path if you want to save the comparison. The results will be 
#' saved in a folder named by `output_folder_name` parameter. The files created will be saved in 
#' xlsx format.
#' @param output_folder_name The folder name where the comparison will be saved. If not specified,
#' the folder name will be "Sistec". This parameter is useless if `output_path` was not specified.
#' 
#' @export
compare_sistec <- function (x, sistec,
                            write_output_path = NULL,
                            institute = "Sistec") {
  UseMethod("compare_sistec", x)
}

compare_sistec.qacademico_data_frame <- function(qacademico, sistec){
  compare_sistec_qacademico(qacademico, sistec)
}

