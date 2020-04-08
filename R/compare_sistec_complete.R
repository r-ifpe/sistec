#' @export
compare_sistec_qacademico_complete <- function(sistec_path, qacademico_path,
                                               type = "complete"){
  
  # read files
  sistec <- read_sistec(path = sistec_path, type = type)
  qacademico <- read_qacademico(path = qacademico_path, type = type)

  # remove invalid cpf
  data <- filter_cpf_sistec(sistec)
  sistec <- data$sistec
  sistec_without_cpf <- data$sistec_without_cpf
  
  data <- filter_cpf_qacademico(qacademico)
  qacademico <- data$qacademico
  qacademico_without_cpf <- data$qacademico_without_cpf
  
  # remove uninked cpf
  data <- unlinked_students_sistec_qacademico(sistec, qacademico)
  sistec <- data$sistec
  sistec_without_qacademico <- data$sistec_without_qacademico
  qacademico <- data$qacademico
  qacademico_without_sistec <- data$qacademico_without_sistec
  
  # join qacademico and sistec
  ifpe_data <- join_sistec_qacademico(sistec, qacademico)

  # linked courses between qacademico and sistec
  ifpe_data <- linked_courses_sistec_qacademico(ifpe_data)
  
  browser()
  # remove cases without link
  qacademico_without_link <- dplyr::anti_join(qacademico, ifpe_dados,
                                              by = c("Cpf", "Curso" = "Curso_q"))
  
  sistec_without_link <- dplyr::anti_join(sistec, ifpe_dados,
                                          by = c("NU_CPF" = "Cpf", "NO_CURSO" = "Curso_sistec"))
  
  qacademico <- dplyr::anti_join(qacademico, qacademico_without_link, by = "Cpf")
  sistec <- dplyr::anti_join(sistec, sistec_without_link, by = "NU_CPF")  
  
  browser()
  ifpe_dados$Status <- comparar_situacao(ifpe_dados$Status_sistec, ifpe_dados$Status_q_mat)
  
  ciclos <- ifpe_dados$Ciclo_sistec %>% unique() %>% stats::na.omit()
  browser() 
  
  a <- lapply(1:length(ciclos), function(e){
    ifpe_dados %>%
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


