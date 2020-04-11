#' @importFrom rlang sym
filter_cpf_sistec <- function(x){
  sistec_without_cpf <- dplyr::filter(x, !!sym("NU_CPF") == "")
  sistec <- dplyr::filter(x, !!sym("NU_CPF") != "") 
  
  list(sistec = sistec, sistec_without_cpf = sistec_without_cpf)
}