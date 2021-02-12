aria_head_tags <- function() {
  shiny::HTML(
    ".shiny-input-container {margin-bottom: 0px} 
  .progress.active.shiny-file-input-progress { margin-bottom: 0px } 
  #year {margin-botton: 20px} 
  .checkbox { margin-top: 0px}"
  )
}

aria_version <- function() {
  description_path <- system.file("DESCRIPTION", package = "sistec")
  as.character(read.dcf(description_path, fields = "Version"))
}

aria_run <- function(ui, server, version, options_port, options_launch_browser) {
  if (version == "desktop") {
    shiny::shinyApp(ui, server,
      options = list(port = options_port, launch.browser = options_launch_browser)
    )
  } else {
    shiny::shinyApp(ui, server)
  }
}

server_input_path <- function(input_path) {
  slash <- stringr::str_locate_all(input_path[1], "/")
  last_slash <- slash[[1]][nrow(slash[[1]]), 2]
  substr(input_path[1], 1, last_slash)
}

generate_session_id <- function() {
  clock_session <- Sys.time()

  paste0(
    stringr::str_sub(clock_session, 1, 4),
    stringr::str_sub(clock_session, 6, 7),
    stringr::str_sub(clock_session, 9, 10),
    stringr::str_sub(clock_session, 12, 13),
    stringr::str_sub(clock_session, 15, 16),
    floor(stats::runif(1, 0.1, 0.9999) * 1000000)
  )
}

shiny_output_path <- function(output_path) {
  if (is.null(output_path)) {
    if_windows <- tolower(Sys.getenv("SystemRoot"))
    if (grepl("windows", if_windows)) {
      output_path <- utils::choose.dir()
      output_path <- gsub("\\\\", "/", output_path)
    } else {
      output_path <- tcltk::tk_choose.dir()
    }
  } else {
    if_windows <- tolower(Sys.getenv("SystemRoot"))
    if (grepl("windows", if_windows)) {
      output_path <- gsub("\\\\", "/", output_path)
    }
  }
  output_path
}

create_zipped_file <- function(file, aria_values) {
  owd <- setwd(aria_values$temp_dir)
  on.exit(setwd(owd))
  files <- NULL

  write_output(
    aria_values$comparison,
    output_path = paste0(aria_values$temp_dir, "/", aria_values$session_id),
    output_folder_name = "ARIA"
  )

  if_windows <- tolower(Sys.getenv("SystemRoot"))
  if (grepl("windows", if_windows)) {
    shell(paste0(
      "cd ", aria_values$session_id, "&& zip -r ",
      file, " ARIA"
    ))
  } else {
    system(paste0(
      "cd ", aria_values$session_id, "; zip -r ",
      file, " ARIA"
    ))
  }
}

aria_desktop_download_files <- function(aria_values) {
  if (is.list(aria_values$comparison)) {
    if_windows <- tolower(Sys.getenv("SystemRoot"))
    if (grepl("windows", if_windows)) {
      output_path <- utils::choose.dir()
      output_path <- gsub("\\\\", "/", output_path)
    } else {
      output_path <- tcltk::tk_choose.dir()
    }

    write_output(
      aria_values$comparison,
      output_path = output_path,
      output_folder_name = "ARIA"
    )

    "Download realizado com sucesso!"
  } else {
    ""
  }
}

delete_aria_files <- function(aria_values) {
  unlink(paste0(aria_values$temp_dir, "/", aria_values$session_id), recursive = TRUE)
}
