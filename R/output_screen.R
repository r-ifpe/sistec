output_screen <- function(input_sistec, input_rfept,
                          comparison){

  if(!is.null(input_sistec) && !is.null(input_rfept)){
    response <- compare_screen(comparison)
  } else if(is.null(input_sistec) && is.null(input_rfept)){
    response <- "Selecione os arquivos do Sistec e do registro acad\u00eamico."   
  } else if(is.null(input_rfept)){
    response <- "Selecione os arquivos do registro acad\u00eamico."
  } else if(is.null(input_sistec)){
    response <- "Selecione os arquivos do sistec."
  } 
  
  response
}

compare_screen <- function(comparison){

 rfept <- rfept_table(comparison$rfept_complete)  
 shiny::HTML(paste(paste0("Compara\u00e7\u00e3o entre Sistec e ", rfept,
                          " realizada com sucesso!"),
                    "", "",
                    "Alunos sem CPF:",
                    paste0("&emsp; - Sistec: ", nrow(comparison$sistec_without_cpf)),
                    paste0("&emsp; - ", rfept, ": ", nrow(comparison$rfept_without_cpf)),
                    "V\u00ednculos n\u00e3o encontrados:",
                    paste0("&emsp; - Sistec: ", nrow(comparison$sistec_without_rfept)),
                    paste0("&emsp; - ", rfept, ": ", nrow(comparison$rfept_without_sistec)),
                    "Erro no cadastro:",
                    paste0("&emsp; - Na data de in\u00edcio: ", nrow(comparison$rfept_wrong_beginning)),
                    paste0("&emsp; - No ciclo: ", nrow(comparison$rfept_wrong_cyclo)),
                    "Situa\u00e7\u00f5es comparadas:",
                    paste0("&emsp; - Atualizadas: ", nrow(comparison$situation_updated)),
                    paste0("&emsp; - Desatualizadas: ", nrow(comparison$situation_to_update)),
                    sep = '<br/>'))
}
