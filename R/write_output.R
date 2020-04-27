#' Save the comparison results
#'
#' You can use this function to save the results saparated by campus. The resutls will be
#' saved in .xlsx format.
#'
#' @param output_path The folder where you want to save the results.
#' @param output_folder_name The folder's name you want to save the results.
#' @param comparison A data frame returned by compare_sistec().
#' 
#' @export
write_output <- function(output_path, 
                         output_folder_name = "Sistec_app",
                         comparison){

# write results
if(!is.null(output_path)) {
  path <- paste0(output_path, "/", output_folder_name)
  
  write_sistec(comparison$sistec_without_cpf, path,
               "Retificar CPF/Sistec", "sem cpf")
  write_sistec(comparison$sistec_without_qacademico, path,
               "Inserir no Qacademico", "sem qacademico")
  
  write_qacademico(comparison$qacademico_without_cpf, path,
                   "Retificar CPF/Qacademico", "sem cpf")
  write_qacademico(comparison$qacademico_without_sistec, path,
                   "Inserir no Sistec", "sem sistec")
  
  write_status_comparison(comparison$situation_to_update, path,
                          "Retificar Situa\u00e7\u00e3o", "alterar situa\u00e7\u00e3o") 
}
}