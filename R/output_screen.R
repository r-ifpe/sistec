output_screen <- function(input_sistec, input_qacademico,
                          comparison){
  
  if(!is.null(input_qacademico) && !is.null(input_sistec)){
    response <- qacademico_screen(comparison)
  } else if(is.null(input_qacademico) && is.null(input_sistec)){
    response <- "Selecione os arquivos do Qacademico e Sistec."   
  } else if(is.null(input_qacademico)){
    response <- "Selecione os arquivos do qacademico."
  } else if(is.null(input_sistec)){
    response <- "Selecione os arquivos do sistec."
  } 
  
  response
}


qacademico_screen <- function(comparison){

 shiny::HTML(paste("Compara\u00e7\u00e3o entre Sistec e Qacademico realizada com sucesso!", # Comparação
                    "", "",
                    "Alunos sem CPF:",
                    paste0("&emsp; - Sistec: ", nrow(comparison$sistec_without_cpf)),
                    paste0("&emsp; - Qacademico: ", nrow(comparison$qacademico_without_cpf)),
                    "V\u00ednculos n\u00e3o encontrados:",
                    paste0("&emsp; - Sistec: ", nrow(comparison$sistec_without_qacademico)),
                    paste0("&emsp; - Qacademico: ", nrow(comparison$qacademico_without_sistec)),
                    "Situa\u00e7\u00f5es comparadas:",
                    paste0("&emsp; - Atualizadas: ", sum(comparison$situation_to_update$Status)),
                    paste0("&emsp; - Desatualizadas: ", sum(!comparison$situation_to_update$Status)),
                    sep = '<br/>'))
}

