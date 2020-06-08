#' Indentify and read files
#'
#' This function support two kinds of schemas: from the api and the website. See Details 
#' if you need help to download the Qacademico data.
#'
#' @param path The Sigaa file's path. 
#' @return A data frame.
#' 
#' @details  AINDA FALTA FAZER PARA O SIGAA. To download the student's data, go to your proper account on Qacademico and 
#' follow:
#'  
#' - "Relatorio de Alunos" -> "Listagem de Alunos" (choose year and period)
#' - Click on "visualizar" 
#' - Using F10 shortcut and save in .csv format
#' - Rename the including year and period (example2020_1.csv) 
#'
#' Be sure that your data has the variables: "Matricula", "Nome", "Situacao Matricula", 
#' "Curso", "Cpf", "Instituicao", "Per. Letivo Inicial".
#' 
#' @export
read_rfept <- function(path = ""){
  
  if(path == "") stop("You need to specify the path.")
  
  files <- list.files(path = path, pattern = "*.csv")
  file <- paste0(path , "/", files[1])
  
  qacademico <- grepl("Per. Letivo Inicial", readLines(file, n = 1))
  
  if(qacademico){
    read_qacademico(path)
  } else {
    read_sigaa(path)
  }
}