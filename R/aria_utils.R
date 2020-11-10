aria_version <- function(){
  description_path <- system.file("DESCRIPTION", package = "sistec")
  as.character(read.dcf(description_path, fields = "Version"))
}

aria_run <- function(ui, server, version, options_port, options_launch_browser){
  if(version == "desktop"){
    shinyApp(ui, server, options = list(port = options_port,
                                        launch.browser = options_launch_browser))
  } else {
    shinyApp(ui, server)
  }
}
