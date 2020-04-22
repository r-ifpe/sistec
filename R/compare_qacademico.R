# this function will be deprecated
compare_sistec_qacademico_simplified <- function(sistec_path, qacademico_path){

  sistec <- read_sistec(path = sistec_path)
  qacademico <- read_qacademico(path = qacademico_path)

  ifpe_dados <- dplyr::full_join(qacademico, sistec, by = c("Cpf" = "NU_CPF")) %>%
    dplyr::transmute(Nome_q = !!sym("Nome"), 
                     Nome_sistec = !!sym("NO_ALUNO"), 
                     Cpf = !!sym("Cpf"),
                     Ciclo = !!sym("CO_CICLO_MATRICULA"),
                     Status_q = !!sym("Situa\u00e7\u00e3o.Matr\u00edcula"), # Situação.Matrícula 
                     Status_sistec = !!sym("NO_STATUS_MATRICULA"))
  
  #true <- comparar_situacao(ifpe_dados$`Status_sistec`, ifpe_dados$`Status_q`)
  true <- compare_situation_sistec_qacademico(ifpe_dados$`Status_sistec`,
                                              ifpe_dados$`Status_q`)
  
  ifpe_dados$Status <- true
  ciclos <- ifpe_dados$Ciclo %>% unique() %>% stats::na.omit()
  
  # verifying students with multi-bond
  dados <- multi_vinculo(ifpe_dados)

  a <- lapply(1:length(ciclos), function(e){
    dados$ifpe_dados %>%
      dplyr::filter(!!sym("Ciclo") == ciclos[e]) %>%
      dplyr::filter(!!sym("Status") == FALSE) %>% # Situação
      dplyr::arrange(!!sym("Status_sistec")) %>% # Situação_sistec
      dplyr::select(-!!sym("Status")) # Situação
  })

  names(a) <- ciclos
  if(nrow(dados$multi_vinculo) > 0){
    a$multi_vinculo <- dados$multi_vinculo
  }

 list(ifpe_dados = ifpe_dados, situation = a)
}
