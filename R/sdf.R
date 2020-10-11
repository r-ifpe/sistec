
#' @param oppa_url 
#'
#' @param dest_folder 
#' 
#' @examples 
#'
#' @importFrom utils download.file
#' @export
baixar_arquivos <- function(dest_folder = NULL){
  utils::download.file("oppa_url", 
                       destfile = dest_folder,
                       method = "wget")
  
}


