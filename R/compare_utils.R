create_sistec_rfept_list <- function(sistec, rfept){
  
  list(sistec = sistec,
       sistec_complete = sistec,
       sistec_without_cpf = data.frame(),
       sistec_without_rfept = data.frame(),
       rfept = rfept,
       rfept_complete = rfept,
       rfept_without_cpf = data.frame(),
       rfept_without_sistec = data.frame(),
       situation_updated = data.frame(),
       situation_to_update = data.frame(),
       sistec_rfept_linked = data.frame(),
       linked_courses = data.frame())
}

#' @importFrom rlang sym
remove_invalid_cpf <- function(x){
  
  rfept_invalid <- c("", "   .   .   -  ", "___.___.___-__")
  x$rfept_without_cpf <- dplyr::filter(x$rfept, !!sym("R_NU_CPF") %in% rfept_invalid)
  x$rfept <- dplyr::filter(x$rfept, !(!!sym("R_NU_CPF") %in% rfept_invalid))
  
  sistec_invalid <- c("000.000.000-00")
  x$sistec_without_cpf <- dplyr::filter(x$sistec, !!sym("S_NU_CPF") == sistec_invalid)
  x$sistec <- dplyr::filter(x$sistec, !!sym("S_NU_CPF") != sistec_invalid) 
  
  x
}

remove_unliked_cpf <- function(x){
  x$rfept_without_sistec <- dplyr::anti_join(x$rfept, x$sistec,
                                             by = c("R_NU_CPF" = "S_NU_CPF"))
  x$sistec_without_rfept <- dplyr::anti_join(x$sistec, x$rfept,
                                             by = c("S_NU_CPF" = "R_NU_CPF"))
  
  x$rfept <- dplyr::anti_join(x$rfept, x$rfept_without_sistec, by = "R_NU_CPF")
  x$sistec <- dplyr::anti_join(x$sistec, x$sistec_without_rfept, by = "S_NU_CPF")
  
  x
}


#' @importFrom stringr str_detect
compare_situation <- function(x){

  sistec <- x$sistec_rfept_linked$S_NO_STATUS_MATRICULA
  rfept <- x$sistec_rfept_linked$R_NO_STATUS_MATRICULA
  
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
  x$sistec_rfept_linked$S_NO_STATUS_IGUAL <- status
  x
}

split_situation <- function(x){

  x$situation_updated <- x$sistec_rfept_linked %>% 
    dplyr::filter( !!sym("S_NO_STATUS_IGUAL") == TRUE)
  
  x$situation_to_update <- x$sistec_rfept_linked %>% 
    dplyr::filter( !!sym("S_NO_STATUS_IGUAL") == FALSE)
  
  # there is no reason to use this table because 
  # the situation were already divided
  x$sistec_rfept_linked <- NULL # clean memory
  
  x
}

#' @importFrom dplyr %>% 
#' @importFrom rlang syms
linked_courses_data_frame <- function(x){

  select_vars <- c("R_DT_INICIO_CURSO", "R_NO_CURSO", "S_NO_CURSO_LINKED", "S_CO_CICLO_MATRICULA")
  arrange_vars <- c("R_NO_CURSO", "R_DT_INICIO_CURSO")
  
  x$linked_courses <- x$situation_updated %>% 
    dplyr::select(!!!syms(select_vars)) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(!!!syms(arrange_vars))
  
  x
}
