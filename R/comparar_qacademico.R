#' @importFrom rlang sym
#' @export
compare_sistec_qacademico <- function(sistec_path, qacademico_path, path = "arquivos/"){

  sistec <- read_sistec(path = sistec_path)
  qacademico <- read_qacademico(path = qacademico_path)
  
  ifpe_dados <- dplyr::full_join(qacademico, sistec, by = c("Cpf" = "NU_CPF")) %>%
    dplyr::transmute(Nome_q = !!sym("Nome"), 
                     Nome_sistec = !!sym("NO_ALUNO"), 
                     Cpf = !!sym("Cpf"),
                     Ciclo = !!sym("CO_CICLO_MATRICULA"),
                     `Situação_q` = !!sym("Situação.Matrícula"), # Situação.Matrícula 
                     `Situação_sistec` = !!sym("NO_STATUS_MATRICULA"))

  true <- comparar_situacao(ifpe_dados$`Situação_sistec`, ifpe_dados$`Situação_q`)

  ifpe_dados$`Situação` <- true
  ciclos <- ifpe_dados$Ciclo %>% unique() %>% stats::na.omit()
  
  # verifying students with multi-bond
  dados <- multi_vinculo(ifpe_dados)

  a <- lapply(1:length(ciclos), function(e){
    dados$ifpe_dados %>%
      dplyr::filter(!!sym("Ciclo") == ciclos[e]) %>%
      dplyr::filter(!!sym("Situação") == FALSE) %>% # Situação
      dplyr::arrange(!!sym("Situação_sistec")) %>% # Situação_sistec
      dplyr::select(-!!sym("Situação")) # Situação
  })

  names(a) <- ciclos
  if(nrow(dados$multi_vinculo) > 0){
    a$multi_vinculo <- dados$multi_vinculo
  }

 list(ifpe_dados = ifpe_dados, situation = a)
}

#' @importFrom stringr str_detect
#' @export
comparar_situacao <- function(sistec, qacademico){

  # existe_qacademico <- !is.na(qacademico)
  status_concluido <- str_detect(sistec, "CONCLUÍDA") &
    str_detect(qacademico, "Concluído|Formado")
  status_integralizada <- str_detect(sistec, "INTEGRALIZADA") &
    str_detect(qacademico, "Concludente|ENADE|Vínculo")
  status_abandono <- str_detect(sistec, "ABANDONO") &
    str_detect(qacademico, "Abandono")
  status_desligado <- str_detect(sistec, "DESLIGADO") &
    str_detect(qacademico, "Cancelamento|Jubilado")
  status_em_curso <- str_detect(sistec, "EM_CURSO") &
    str_detect(qacademico, "Matriculado|Trancado")
  status_transferido <- str_detect(sistec, "TRANSF_EXT") &
    str_detect(qacademico, "Transferido Externo")

status <- status_abandono | status_concluido | status_integralizada | status_desligado |
  status_em_curso | status_transferido

status[is.na(status)] <- FALSE
status
}

num_para_cpf <- function(num) {

  stringr::str_replace(string = num,
              pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
              replacement = "\\1.\\2.\\3-")
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