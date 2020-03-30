#' Qacademico screen 
#' 
#' This function runs the comparison beteween sistec and qacademico, make the download of the 
#' results and plot screen results in the app. 
#'
#' @param input_qacademico A list of qacademico's files path. This list, normally, comes from the app.
#' @param input_sistec A list of sistec's files path. This list, normally, comes from the app.
#'
#' @export
output_screen <- function(input_qacademico, input_sistec){
  
  if(!is.null(input_qacademico) && !is.null(input_sistec)){
    response <- comparar_tudo(input_qacademico, input_sistec)
  } else if(is.null(input_qacademico) && is.null(input_sistec)){
    response <- "Selecione os arquivos do Qacademico e Sistec."   
  } else if(is.null(input_qacademico)){
    response <- "Selecione os arquivos do qacademico."
  } else if(is.null(input_sistec)){
    response <- "Selecione os arquivos do sistec."
  } 
  
  response
}


comparar_tudo <- function(input_qacademico,input_sistec ){

  qacademico_path <- server_input_path(input_qacademico)
  sistec_path <- server_input_path(input_sistec)

  table_compared <- compare_sistec_qacademico(qacademico_path = qacademico_path,
                                              sistec_path = sistec_path)
  
  openxlsx::write.xlsx(table_compared$situation, "situa\u00e7\u00e3o.xlsx") # situação.xlsx
  
  total_students <- nrow(table_compared$ifpe_dados)
  multi_vinculo <- nrow(table_compared$situation$multi_vinculo)
  
  students_to_update <- sum(unlist(
    lapply(table_compared$situation, nrow))) - multi_vinculo
  
  students_updated <- total_students - students_to_update - multi_vinculo
  
  shiny::HTML(paste("Compara\u00e7\u00e3o entre Qacademico e Sistec realizada com sucesso!", # Comparação
                    "", "", "", 
                    paste0("Situa\u00e7\u00f5es comparadas: ", total_students), # Situações
                    paste0("Alunos atualizados: ", students_updated,
                          " (", round(100*students_updated/total_students, 2 ), "%)"),
                    paste("Alunos com mais de um v\u00edncul0:", multi_vinculo), sep = '<br/>')) # vínculo   
}


