#' Build ARIA desktop version
#' 
#' Build the desktop version of ARIA automatically. You will need 
#' \href{https://jrsoftware.org/isdl.php}{innosetup} to build the executable. 
#' Only works in windows. See Details for more information.  
#'
#' @param aria_folder The folder you want to create the executable.
#' @param download_dependencies Download all the dependencies: r-portable and packages 
#' needed to run the ARIA. Use FALSE if you already have the dependencies in the 
#' aria_folder.
#'
#' @return Create the setup to build ARIA desktop in your specified folder.
#' 
#' @details To create the executable correctly, follow theses steps:
#' 
#' - Build the sistec package (in Rstudio, you use ctrl + shift + b)
#' - Create a folder 
#' - Use `aria_desktop_build("your folder")`
#' - Install innosetup (if you don't have it installed yet)
#' - Double click the .iss file created in your folder and run it.
#' 
#' @examples 
#' \dontrun{
#' aria_folder <- tempdir()
#' aria_desktop_build(aria_folder)
#' }
#' @export
aria_desktop_build <- function(aria_folder = "", 
                               download_dependencies = TRUE){
  
  aria_folder_exists(aria_folder)
  aria_folder <- aria_folder_remove_slash(aria_folder)
  
  if(download_dependencies){
    aria_desktop_download_dependencies(aria_folder) 
  }
  
  aria_desktop_create_iss(aria_folder)
  aria_desktop_copy_package(aria_folder)
}

aria_desktop_download_dependencies <- function(aria_folder = ""){
  
  aria_folder_exists(aria_folder)
  utils::download.file("https://www.dropbox.com/s/m2ekuqb0crze9l8/ARIA_desktop.zip?dl=1",
                       destfile = paste0(aria_folder, "/ARIA_desktop.zip"),
                       mode = "wb")
  utils::untar(paste0(aria_folder, "/ARIA_desktop.zip"), exdir = aria_folder)
  unlink(paste0(aria_folder, "/ARIA_desktop.zip"))
}

aria_desktop_create_iss <- function(aria_folder = ""){
  
  aria_folder_exists(aria_folder)
  r_portable_exists(aria_folder)

  inno_file <- readLines("inst/extdata/desktop_version/ARIA_desktop.iss")
  aria_desktop_folder <- stringr::str_replace_all(aria_folder, "/", "\\\\")
  aria_desktop_folder <- paste0(aria_desktop_folder, "\\ARIA_desktop\\")
  output_folder <- stringr::str_replace_all(aria_folder, "/", "\\\\")
  aria_version <- aria_version()
  
  inno_file[9] <- paste0("AppVersion=", as.character(aria_version))
  inno_file[18] <- paste0("OutputDir=", output_folder)
  inno_file[20] <- paste0("SetupIconFile=", aria_desktop_folder, "LogoIFPE.ico") 
  inno_file[32] <- paste0('Source: "', aria_desktop_folder,
                          'ARIA.bat"; DestDir: "{app}"; Flags: ignoreversion')
  inno_file[33] <- paste0('Source: "', aria_desktop_folder, 
                          '*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs')
  
  utils::write.table(inno_file, file = paste0(aria_folder, "\\ARIA.iss"),
                     row.names = FALSE, quote = FALSE, col.names = FALSE)
}

aria_desktop_copy_package <- function(aria_folder = ""){
  
  aria_folder_exists(aria_folder)
  
  unlink(paste0(aria_folder, "/ARIA_desktop/R-Portable/App/R-Portable/library/sistec"),
         recursive = TRUE)

  file.copy(system.file(package = "sistec"), 
            paste0(aria_folder, "/ARIA_desktop/R-Portable/App/R-Portable/library"),
            recursive = TRUE)
}

r_portable_exists <- function(aria_folder){
  r_portable_exists <- file.exists(paste0(aria_folder,"/ARIA_desktop/R-Portable/R-Portable.exe"))
    
  if(!r_portable_exists){
    stop("R-Portable not found in ARIA desktop folder.")
  }
}
  
aria_folder_exists <- function(aria_folder = ""){
  if(aria_folder == ""){
    stop("You need to select a folder.")
  }
}

aria_folder_remove_slash <- function(aria_folder){
  stringr::str_remove(aria_folder, "/$|\\\\$")
}

 