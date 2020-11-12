server_input_path <- function(input_path){
  slash <- stringr::str_locate_all(input_path[1], "/")
  last_slash <- slash[[1]][nrow(slash[[1]]), 2]
  substr(input_path[1], 1, last_slash)
}

shiny_comparison <- function(sistec_path, rfept_path, year){
  sistec <- read_sistec(server_input_path(sistec_path), year)
  rfept <- read_rfept(server_input_path(rfept_path), year)
  
  compare_sistec(sistec, rfept)
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
