
comparar_q_sistec <- function(path = "arquivos/"){

  library(dplyr)
  library(stringr)
  library(openxlsx)

  #browser()
  sistec <- ler_sistec(path = path)
  qacademico <- ler_qacademico(path = path)

  ifpe_dados <- dplyr::full_join(qacademico, sistec, by = c("Cpf" = "NU_CPF")) %>%
    dplyr::select(Nome, NO_ALUNO, Cpf, CO_CICLO_MATRICULA, `Situação.Matrícula`, NO_STATUS_MATRICULA) %>%
    rename(Nome_q = Nome, Nome_sistec = NO_ALUNO, Cpf = Cpf,
           Ciclo = CO_CICLO_MATRICULA,
           `Situação_q` = `Situação.Matrícula`, `Situação_sistec` = NO_STATUS_MATRICULA)

  true <- comparar_situacao(ifpe_dados$`Situação_sistec`, ifpe_dados$`Situação_q`)

  ifpe_dados$`Situação` <- true
  ciclos <- ifpe_dados$Ciclo %>% unique() %>% na.omit()

  a <- lapply(1:length(ciclos), function(e){
    ifpe_dados %>%
      filter(Ciclo == ciclos[e]) %>%
      arrange(`Situação`)
  })
  
  names(a) <- ciclos
  openxlsx::write.xlsx(a, "q_sistec.xlsx")
}

ler_sistec <- function(path = "extdata"){
    temp = list.files(path = path, pattern = "*.csv")
    temp <- paste0(path, "/", temp)
    sistec <- lapply(temp, utils::read.csv,
                     sep = ";",  stringsAsFactors=FALSE, colClasses =rep("character", 8)) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(NO_STATUS_MATRICULA = ifelse(
        stringr::str_detect(NO_STATUS_MATRICULA, "CONCLU\xcdDA"), "CONCLUÍDA", NO_STATUS_MATRICULA),
                    NU_CPF = num_para_cpf(NU_CPF)) %>%
      dplyr::select(NO_ALUNO, NU_CPF, CO_CICLO_MATRICULA, NO_STATUS_MATRICULA)
  }

ler_qacademico <- function(path = "extdata"){
   temp = list.files(path = path, pattern = "*.xlsx")
   temp <- paste0(path , "/", temp)
   lapply(temp, openxlsx::read.xlsx) %>%
     dplyr::bind_rows() %>%
     dplyr::mutate(Cpf= num_para_cpf(Cpf))
 }



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

num_para_cpf <- function(num) {

  stringr::str_replace(string = num,
              pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
              replacement = "\\1.\\2.\\3-")
}

