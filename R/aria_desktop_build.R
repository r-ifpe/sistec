#' @export
aria_desktop_build <- function(aria_desktop_folder = "",
                               output_folder = ""){
  
  temp_dir <- tempdir()
  
  
  utils::download.file("https://www.dropbox.com/s/k2jbujt4uup0xlp/ARIA_desktop.rar?dl=1",
                       destfile = )
 
  r_portable_exists <- file.exists("C:/Users/dmmad/Desktop/ARIA_desktop/R-Portable/R-Portable.exe")
  if(!r_portable_exists){
    stop(paste0("R-Portable not found in ARIA desktop folder. ",
                "Please install R-Portable or use sistec::install_r_portable()"))
  }
  
  
  inno_file <- readLines("inst/extdata/desktop_version/ARIA_desktop.iss")
  aria_desktop_folder <- stringr::str_replace_all(aria_desktop_folder, "/", "\\\\")
  output_folder <- stringr::str_replace_all(output_folder, "/", "\\\\")
  aria_version <- aria_version()
  
  inno_file[9] <- paste0("AppVersion=", as.character(aria_version))
  inno_file[18] <- paste0("OutputDir=", output_folder)
  inno_file[20] <- paste0("SetupIconFile=", aria_desktop_folder, "\\LogoIFPE.ico") 
  inno_file[32] <- paste0('Source: "', aria_desktop_folder,
                          '\\ARIA.bat"; DestDir: "{app}"; Flags: ignoreversion')
  inno_file[33] <- paste0('Source: "', aria_desktop_folder, 
                          '\\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs')
  
  write.table(inno_file, file = paste0(aria_desktop_folder, "\\ARIA.iss"),
              row.names = FALSE, quote = FALSE, col.names = FALSE)
}

#' @export
aria_desktop_iss <- function(aria_desktop_folder = "",
                             output_folder = ""){
  
  r_portable_exists <- file.exists("C:/Users/dmmad/Desktop/ARIA_desktop/R-Portable/R-Portable.exe")
  if(!r_portable_exists){
    stop(paste0("R-Portable not found in ARIA desktop folder. ",
                "Please install R-Portable or use sistec::install_r_portable()"))
  }
  
  
  inno_file <- readLines("inst/extdata/desktop_version/ARIA_desktop.iss")
  aria_desktop_folder <- stringr::str_replace_all(aria_desktop_folder, "/", "\\\\")
  output_folder <- stringr::str_replace_all(output_folder, "/", "\\\\")
  aria_version <- aria_version()
  
  inno_file[9] <- paste0("AppVersion=", as.character(aria_version))
  inno_file[18] <- paste0("OutputDir=", output_folder)
  inno_file[20] <- paste0("SetupIconFile=", aria_desktop_folder, "\\LogoIFPE.ico") 
  inno_file[32] <- paste0('Source: "', aria_desktop_folder,
                          '\\ARIA.bat"; DestDir: "{app}"; Flags: ignoreversion')
  inno_file[33] <- paste0('Source: "', aria_desktop_folder, 
                          '\\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs')
  
  write.table(inno_file, file = paste0(aria_desktop_folder, "\\ARIA.iss"),
              row.names = FALSE, quote = FALSE, col.names = FALSE)
}

