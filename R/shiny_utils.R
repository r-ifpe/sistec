read_period_input <- function(){
  utils::read.csv(system.file("extdata/shiny/period_input.csv", package = "sistec"),
                  header = TRUE, colClasses = "character")
}

server_input_path <- function(input_path){
  slash <- stringr::str_locate_all(input_path[1], "/")
  last_slash <- slash[[1]][nrow(slash[[1]]), 2]
  substr(input_path[1], 1, last_slash)
}

shiny_comparison <- function(sistec_path, rfept_path, year){
  sistec <- read_sistec(server_input_path(sistec_path), year)
  rfept <- read_rfept(server_input_path(rfept_path), year)
  
  compare_sistec(sistec, rfept)

#linked_course_approach (parameter)
#   if(linked_course_approach$tech == "aria"){
#     compare_sistec(sistec, rfept)
#   } else {
#     linked_course <- read_linked_courses(server_input_path(linked_course_approach$path))
#     compare_sistec(sistec, rfept, linked_course) 
#   }
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

check_linked_course_approach <- function(datapath, estimate_by_aria){
  
  linked_course_approach <- list()
  
  if(!is.null(datapath)){
    linked_course_approach$exist = TRUE
    linked_course_approach$tech = "linked_course_file"
    linked_course_approach$path = datapath
  } else if(estimate_by_aria){
    linked_course_approach$exist = TRUE
    linked_course_approach$tech = "aria"
  } else {
    linked_course_approach$exist = FALSE
  }
  
  linked_course_approach
}











