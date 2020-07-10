read_linked_courses <- function(path = ""){
  
  if(path == "") stop("You need to specify the path.")
  
  temp = list.files(path = path, pattern = "*.xlsx")
  temp <- paste0(path, "/", temp)
  
  openxlsx::read.xlsx(temp)
}