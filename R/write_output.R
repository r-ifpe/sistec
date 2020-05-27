#' Save the comparison results
#'
#' You can use this function to save the results saparated by campus. The resutls will be
#' saved in .xlsx format.
#'
#' @param x A list returned by `compare_sistec()` or a data frame reutned by
#' @param output_path The folder where you want to save the results.
#' @param output_folder_name The folder's name you want to save the results.
#' 
#' @return None.
#' 
#' @export
write_output <- function(x,
                         output_path = NULL, 
                         output_folder_name = "Sistec_app"){
  UseMethod("write_output")
}

#' @export
write_output.comparison_list <- function(x, 
                                         output_path = NULL, 
                                         output_folder_name = "Sistec_app"){
  # write results
  if(!is.null(output_path)) {
    path <- paste0(output_path, "/", output_folder_name)
    write_comparison_list(x, path)
    write_comparison_tables(x, path)
  } else {
    stop("Please, select a folder to download the results.")
  }
}
