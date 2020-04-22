unlinked_cpf_sistec_qacademico <- function(sistec, qacademico){
  qacademico_without_sistec <- dplyr::anti_join(qacademico, sistec, by = c("Cpf" = "NU_CPF"))
  sistec_without_qacademico <- dplyr::anti_join(sistec, qacademico, by = c("NU_CPF" = "Cpf"))
  
  qacademico <- dplyr::anti_join(qacademico, qacademico_without_sistec, by = "Cpf")
  sistec <- dplyr::anti_join(sistec, sistec_without_qacademico, by = "NU_CPF") 
  
  list(sistec = sistec,
       sistec_without_qacademico = sistec_without_qacademico,
       qacademico = qacademico,
       qacademico_without_sistec = qacademico_without_sistec)
}

#' @importFrom rlang sym
join_sistec_qacademico <- function(sistec, qacademico){
  dplyr::inner_join(qacademico, sistec, by = c("Cpf" = "NU_CPF")) %>%
    dplyr::transmute(Cpf = !!sym("Cpf"),
                     Matricula_q = !!sym("Matr\u00edcula"),
                     Nome_q = !!sym("Nome"), 
                     Status_q = !!sym("Situa\u00e7\u00e3o.Matr\u00edcula"), # Situação.Matrícula 
                     Curso_q = !!sym("Curso"), 
                     Campus_q = !!sym("Campus"),
                     Inicio_q_semestre = !!sym("Per..Letivo.Inicial"),
                     Nome_sistec = !!sym("NO_ALUNO"),
                     Ciclo_sistec = !!sym("CO_CICLO_MATRICULA"),
                     Status_sistec = !!sym("NO_STATUS_MATRICULA"),
                     Curso_sistec = !!sym("NO_CURSO"),
                     Campus_sistec = !!sym("NO_CAMPUS"),
                     Inicio_sistec = !!sym("DT_DATA_INICIO"),
                     Inicio_sistec_semestre = convert_date_sistec_qacademico(!!sym("DT_DATA_INICIO")))
}

#' @importFrom dplyr %>% 
linked_courses_sistec_qacademico <- function(x){
   x %>% 
    dplyr::filter(!!sym("Inicio_q_semestre") == !!sym("Inicio_sistec_semestre")) %>% 
    dplyr::group_by(!!sym("Curso_q"), !!sym("Curso_sistec")) %>% 
    dplyr::tally() %>% 
    dplyr::arrange(!!sym("Curso_q"), dplyr::desc(!!sym("n"))) %>% 
    dplyr::distinct(!!sym("Curso_q"), .keep_all = TRUE) %>% 
    dplyr::rename(Curso_sistec_link = !!sym("Curso_sistec"), Qtd_alunos = !!sym("n")) %>% 
    dplyr::right_join(x, by = "Curso_q") %>% 
    dplyr::filter(!!sym("Curso_sistec") == !!sym("Curso_sistec_link"))
}

#' @importFrom stringr str_detect
compare_situation_sistec_qacademico <- function(sistec, qacademico){
  
  # existe_qacademico <- !is.na(qacademico)
  status_concluido <- str_detect(sistec, "CONCLU\u00cdDA") & # CONCLUÍDA
    str_detect(qacademico, "Conclu\u00eddo|Formado") # Concluído
  status_integralizada <- str_detect(sistec, "INTEGRALIZADA") &
    str_detect(qacademico, "Concludente|ENADE|V\u00ednculo") # Vínculo
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

convert_date_sistec_qacademico <- function(date){
  year <- stringr::str_sub(date, 7, 10)
  month <- as.numeric(stringr::str_sub(date, 4, 5))
  semester <- ifelse(month > 6, 2, 1)
  paste0(year, "/", semester )
  
}


unlinked_course_sistec_qacademico <- function(sistec, qacademico, joined_data){
  sistec_without_link <- dplyr::anti_join(sistec, joined_data,
                                          by = c("NU_CPF" = "Cpf",
                                                 "NO_CURSO" = "Curso_sistec"))
  
  qacademico_without_link <- dplyr::anti_join(qacademico, joined_data,
                                              by = c("Cpf", "Curso" = "Curso_q"))
  
  sistec <- dplyr::anti_join(sistec, sistec_without_link,
                             by = c("NU_CPF", "NO_CURSO"))
  qacademico <- dplyr::anti_join(qacademico, qacademico_without_link,
                                 by = c("Cpf", "Curso"))

  
  list(sistec = sistec,
       sistec_without_link = sistec_without_link,
       qacademico = qacademico,
       qacademico_without_link = qacademico_without_link)
}
 
