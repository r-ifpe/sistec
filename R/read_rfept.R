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