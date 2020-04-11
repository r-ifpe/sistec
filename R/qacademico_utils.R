#' @importFrom rlang sym
filter_cpf_qacademico <- function(x){
  invalid <- c("", "   .   .   -  ", "___.___.___-__")

  qacademico_without_cpf <- dplyr::filter(x, !!sym("Cpf") %in% invalid)
  qacademico <- dplyr::filter(x, !(!!sym("Cpf") %in% invalid))
  
  list(qacademico = qacademico, qacademico_without_cpf = qacademico_without_cpf)
}

#' @importFrom rlang sym
multi_vinculo <- function(x){
  multi_vinculo <- x %>% 
    dplyr::group_by(!!sym("Cpf")) %>% 
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") > 1)
  
  list(ifpe_dados = x %>% 
         dplyr::filter(!(!!sym("Cpf") %in% multi_vinculo$Cpf)),
       multi_vinculo = x %>% 
         dplyr::filter(!!sym("Cpf") %in% multi_vinculo$Cpf)
  )
  
}