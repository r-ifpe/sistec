filter_cpf_sistec <- function(x){
  sistec_without_cpf <- dplyr::filter(x, NU_CPF == "")
  sistec <- dplyr::filter(x, NU_CPF != "") 
  
  list(sistec = sistec, sistec_without_cpf = sistec_without_cpf)
}