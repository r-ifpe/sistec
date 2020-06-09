#' Identify and read academic registration
#'
#' The `read_rfept()` is a wrapper around `read_qacademico()` and `read_sigaa()`. Now 
#' you just need to specify the folder path and `read_rfept()` identifies if it is a 
#' qacademico or sigaa file and then read it.
#'
#' @param path The file's path to Qacademico or Sigaa folder. 
#' @return A data frame.
#' 
#' @details  By now, this function only supports qacademico and sigaa.
#' 
#' @examples 
#' # these datasets are not a real ones. It is just for test purpose.
#' 
#' qacademico <- read_rfept(system.file("extdata/examples/qacademico",
#'                                      package = "sistec")) 
#'                                      
#' sigaa <- read_rfept(system.file("extdata/examples/sigaa",
#'                                  package = "sistec"))
#'                                  
#'  class(qacademico)
#'  class(sigaa)                                 
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