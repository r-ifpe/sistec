aria_logs <- function(session_id, rfept, sistec, comparison, temp_dir, download_count){
  cat(file = stderr(), "====================================================================\n")
  print_logs(session_id, "rfept path = ", rfept)
  print_logs(session_id, "sistec path = ", sistec)
  print_logs(session_id, "comparison class = ", class(comparison))
  print_logs(session_id, "temp folder = ", temp_dir)
  print_logs(session_id, "download count = ", download_count)
  print_logs(session_id, "aria folder exists = ", dir.exists(paste0(temp_dir, "/ARIA")))
}

print_logs <- function(id, text, x){
  cat(file = stderr(), id , ": ", text, x, "\n")
}
