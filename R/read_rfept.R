#' Identify and read academic registration
#'
#' The `read_rfept()` is a wrapper around `read_qacademico()` and `read_sigaa()`. Now 
#' you just need to specify the folder path and `read_rfept()` identifies if it is a 
#' qacademico or sigaa file and then read it.
#'
#' @param path The file's path to Qacademico or Sigaa folder. 
#' @param start A character with the date to start the comparison. The default is the minimum 
#' value found in the data. The date has to be in this format: "yyyy.semester".
#' Ex.: "2019.1" or "2019.2".
#' @return A data frame.
#' 
#' @details  By now, this function only supports qacademico and sigaa-sc.
#' 
#' @examples 
#' # these datasets are not a real ones. It is just for test purpose.
#' 
#' qacademico <- read_rfept(system.file("extdata/examples/qacademico", package = "sistec")) 
#'                                      
#' sigaa <- read_rfept(system.file("extdata/examples/sigaa", package = "sistec"))
#'    
#' class(qacademico)
#' class(sigaa)
#' 
#' # example selecting the period
#' qacademico_2019_2 <- read_rfept(system.file("extdata/examples/qacademico", package = "sistec"),
#'                                 start = "2019.2") 
#' 
#' class(qacademico_2019_2)                                 
#' @export
read_rfept <- function(path = "", start = NULL){
  
  if(path == "") stop("You need to specify the path.")
  
  files <- list.files(path = path, pattern = "*.csv")
  file <- paste0(path , "/", files[1])
  
  qacademico <- stringr::str_detect(readLines(file, n = 1), "Per. Letivo Inicial")

  if(qacademico){
    rfept <- read_qacademico(path, start)
  } else {
    rfept <- read_sigaa(path, start)
  }

  rfept
}

filter_rfept_date <- function(x, start){
  
  year_regex <- "[12][09][0-9]{2}.[12]"

  if(is.null(start)){
    start <- stringr::str_extract(x$R_DT_INICIO_CURSO, year_regex) %>% 
      min(na.rm = TRUE)
  } 

  x %>% 
    dplyr::filter(!!sym("R_DT_INICIO_CURSO") >= start)
}
