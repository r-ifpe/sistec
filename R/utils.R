aria_version <- function(){
  description_path <- system.file("DESCRIPTION", package = "sistec")
  as.character(read.dcf(description_path, fields = "Version"))
}
