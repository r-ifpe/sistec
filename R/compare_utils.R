#' @importFrom rlang sym
filter_cpf_rfept <- function(x){
  invalid <- c("", "   .   .   -  ", "___.___.___-__")
  
  rfept_without_cpf <- dplyr::filter(x, !!sym("R_NU_CPF") %in% invalid)
  rfept <- dplyr::filter(x, !(!!sym("R_NU_CPF") %in% invalid))
  
  list(rfept = rfept, rfept_without_cpf = rfept_without_cpf)
}

#' @importFrom rlang sym
filter_cpf_sistec <- function(x){
  sistec_without_cpf <- dplyr::filter(x, !!sym("S_NU_CPF") == "000.000.000-00")
  sistec <- dplyr::filter(x, !!sym("S_NU_CPF") != "000.000.000-00") 
  
  list(sistec = sistec, sistec_without_cpf = sistec_without_cpf)
}

unlinked_cpf_sistec_rfept <- function(sistec, rfept){
  rfept_without_sistec <- dplyr::anti_join(rfept, sistec, by = c("R_NU_CPF" = "S_NU_CPF"))
  sistec_without_rfept <- dplyr::anti_join(sistec, rfept, by = c("S_NU_CPF" = "R_NU_CPF"))
  
  rfept <- dplyr::anti_join(rfept, rfept_without_sistec, by = "R_NU_CPF")
  sistec <- dplyr::anti_join(sistec, sistec_without_rfept, by = "S_NU_CPF") 
  
  list(sistec = sistec,
       sistec_without_rfept = sistec_without_rfept,
       rfept = rfept,
       rfept_without_sistec = rfept_without_sistec)
}

#' @importFrom rlang sym
join_sistec_rfept <- function(sistec, rfept){
  dplyr::inner_join(sistec, rfept, by = c("S_NU_CPF" = "R_NU_CPF"))
}

#' @importFrom dplyr %>% 
linked_courses_sistec_rfept <- function(x){
  x %>% 
    dplyr::filter(!!sym("R_DT_INICIO_CURSO") == !!sym("S_DT_INICIO_CURSO")) %>% 
    dplyr::group_by(!!sym("R_NO_CURSO"), !!sym("S_NO_CURSO")) %>% 
    dplyr::tally() %>% 
    dplyr::arrange(!!sym("R_NO_CURSO"), dplyr::desc(!!sym("n"))) %>% 
    dplyr::distinct(!!sym("R_NO_CURSO"), .keep_all = TRUE) %>% 
    dplyr::rename(S_NO_CURSO_LINKED = !!sym("S_NO_CURSO"), S_QTD_ALUNOS_LINKED = !!sym("n")) %>% 
    dplyr::right_join(x, by = "R_NO_CURSO") %>% 
    dplyr::filter(!!sym("S_NO_CURSO") == !!sym("S_NO_CURSO_LINKED"))
}

#' @importFrom stringr str_detect
compare_situation_sistec_rfept <- function(situation_to_update){
  
  sistec <- situation_to_update$S_NO_STATUS_MATRICULA
  rfept <- situation_to_update$R_NO_STATUS_MATRICULA
  
  # existe_rfept <- !is.na(rfept)
  status_concluido <- str_detect(sistec, "CONCLU\u00cdDA") & # CONCLUÍDA
    str_detect(rfept, "Conclu\u00eddo|Formado") # Concluído
  status_integralizada <- str_detect(sistec, "INTEGRALIZADA") &
    str_detect(rfept, "Concludente|ENADE|V\u00ednculo") # Vínculo
  status_abandono <- str_detect(sistec, "ABANDONO") &
    str_detect(rfept, "Abandono")
  status_desligado <- str_detect(sistec, "DESLIGADO") &
    str_detect(rfept, "Cancelamento|Jubilado")
  status_em_curso <- str_detect(sistec, "EM_CURSO") &
    str_detect(rfept, "Matriculado|Trancado")
  status_transferido <- str_detect(sistec, "TRANSF_EXT") &
    str_detect(rfept, "Transferido Externo")
  
  status <- status_abandono | status_concluido | status_integralizada | status_desligado |
    status_em_curso | status_transferido
  
  status[is.na(status)] <- FALSE
  status
}

unlinked_course_sistec_rfept <- function(sistec, rfept, joined_data){

  sistec_without_link <- dplyr::anti_join(sistec, joined_data,
                                          by = c("S_NU_CPF", "S_NO_CURSO"))
 
  rfept_without_link <- dplyr::anti_join(rfept, joined_data, # R_NU_CPF is removed in join
                                         by = c("R_NU_CPF" = "S_NU_CPF", "R_NO_CURSO"))
  
  sistec <- dplyr::anti_join(sistec, sistec_without_link,
                             by = c("S_NU_CPF", "S_NO_CURSO"))
  rfept <- dplyr::anti_join(rfept, rfept_without_link,
                            by = c("R_NU_CPF", "R_NO_CURSO"))
  
  list(sistec = sistec,
       sistec_without_link = sistec_without_link,
       rfept = rfept,
       rfept_without_link = rfept_without_link)
}
