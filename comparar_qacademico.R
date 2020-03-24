#' @export
compare_q_sistec <- function(sistec_path, qacademico_path, path = "arquivos/"){

  library(dplyr)
  library(stringr)
  library(openxlsx)

  sistec <- read_sistec(path = sistec_path)
  qacademico <- read_qacademico(path = qacademico_path)

  ifpe_dados <- dplyr::full_join(qacademico, sistec, by = c("Cpf" = "NU_CPF")) %>%
    dplyr::select(Nome, NO_ALUNO, Cpf, CO_CICLO_MATRICULA, `Situação.Matrícula`, NO_STATUS_MATRICULA) %>%
    rename(Nome_q = Nome, Nome_sistec = NO_ALUNO, Cpf = Cpf,
           Ciclo = CO_CICLO_MATRICULA,
           `Situação_q` = `Situação.Matrícula`, `Situação_sistec` = NO_STATUS_MATRICULA)

  true <- comparar_situacao(ifpe_dados$`Situação_sistec`, ifpe_dados$`Situação_q`)

  ifpe_dados$`Situação` <- true
  ciclos <- ifpe_dados$Ciclo %>% unique() %>% na.omit()
  
  # verifying students with multi-bond
  dados <- multi_vinculo(ifpe_dados)

  a <- lapply(1:length(ciclos), function(e){
    dados$ifpe_dados %>%
      filter(Ciclo == ciclos[e]) %>%
      filter(`Situação` == FALSE) %>% 
      arrange(`Situação_sistec`) %>% 
      select(-`Situação`)
  })

  names(a) <- ciclos
  if(nrow(dados$multi_vinculo) > 0){
    a$multi_vinculo <- dados$multi_vinculo
  }

 list(ifpe_dados = ifpe_dados, situation = a)
}

#' @export
comparar_situacao <- function(sistec, qacademico){

  # existe_qacademico <- !is.na(qacademico)
  status_concluido <- stringr::str_detect(sistec, "CONCLUÍDA") &
    str_detect(qacademico, "Concluído|Formado")
  status_integralizada <- stringr::str_detect(sistec, "INTEGRALIZADA") &
    str_detect(qacademico, "Concludente|ENADE|Vínculo")
  status_abandono <- stringr::str_detect(sistec, "ABANDONO") &
    str_detect(qacademico, "Abandono")
  status_desligado <- stringr::str_detect(sistec, "DESLIGADO") &
    str_detect(qacademico, "Cancelamento|Jubilado")
  status_em_curso <- stringr::str_detect(sistec, "EM_CURSO") &
    str_detect(qacademico, "Matriculado|Trancado")
  status_transferido <- stringr::str_detect(sistec, "TRANSF_EXT") &
    str_detect(qacademico, "Transferido Externo")

status <- status_abandono | status_concluido | status_integralizada | status_desligado |
  status_em_curso | status_transferido

status[is.na(status)] <- FALSE
status
}

#' @export
num_para_cpf <- function(num) {

  stringr::str_replace(string = num,
              pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
              replacement = "\\1.\\2.\\3-")
}

#' @export
multi_vinculo <- function(x){
  multi_vinculo <- x %>% 
    group_by(Cpf) %>% 
    tally() %>% 
    filter(n > 1)

  list(ifpe_dados = x %>% 
         filter(!(Cpf %in% multi_vinculo$Cpf)),
       multi_vinculo = x %>% 
         filter(Cpf %in% multi_vinculo$Cpf)
       )

}