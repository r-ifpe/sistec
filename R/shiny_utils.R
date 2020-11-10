server_input_path <- function(input_path){
  slash <- stringr::str_locate_all(input_path[1], "/")
  last_slash <- slash[[1]][nrow(slash[[1]]), 2]
  substr(input_path[1], 1, last_slash)
}

#' @export
shiny_comparison <- function(sistec_path, rfept_path, year){
  sistec <- read_sistec(server_input_path(sistec_path), year)
  rfept <- read_rfept(server_input_path(rfept_path), year)
  
  compare_sistec(sistec, rfept)
}

shiny_output_path <- function(output_path){
  if(is.null(output_path)){
    if_windows <- tolower(Sys.getenv("SystemRoot"))
    if (grepl("windows", if_windows)){
      output_path <- utils::choose.dir()
      output_path <- gsub("\\\\", "/",output_path)
    } else {
      output_path <- tcltk::tk_choose.dir()
    }
  } else {
    if_windows <- tolower(Sys.getenv("SystemRoot"))
    if (grepl("windows", if_windows)){
      output_path <- gsub("\\\\", "/",output_path)
    }
  }
  output_path
}

generate_session_id <- function(){
  clock_session <- Sys.time()
  
  paste0(stringr::str_sub(clock_session, 1, 4),
         stringr::str_sub(clock_session, 6, 7),
         stringr::str_sub(clock_session, 9, 10),
         stringr::str_sub(clock_session, 12, 13),
         stringr::str_sub(clock_session, 15, 16),
         floor(stats::runif(1, 0.1, 0.9999) * 1000000)
  )
}
