create_sistec_rfept_list <- function(sistec, rfept){
  
  list(sistec = sistec,
       sistec_complete = sistec,
       sistec_without_cpf = data.frame(),
       sistec_without_rfept = data.frame(),
       rfept = rfept,
       rfept_complete = rfept,
       rfept_without_cpf = data.frame(),
       rfept_without_sistec = data.frame(),
       rfept_wrong_beginning = data.frame(),
       rfept_wrong_cyclo = data.frame(),
       situation_updated = data.frame(),
       situation_to_update = data.frame(),
       sistec_rfept_linked = data.frame(),
       linked_courses = data.frame())
}

#' @importFrom dplyr sym
remove_invalid_cpf <- function(x){
  
  rfept_invalid <- c("", "   .   .   -  ", "___.___.___-__")
  x$rfept_without_cpf <- dplyr::filter(x$rfept, !!sym("R_NU_CPF") %in% rfept_invalid)
  x$rfept <- dplyr::filter(x$rfept, !(!!sym("R_NU_CPF") %in% rfept_invalid))
  
  sistec_invalid <- c("000.000.000-00")
  x$sistec_without_cpf <- dplyr::filter(x$sistec, !!sym("S_NU_CPF") == sistec_invalid)
  x$sistec <- dplyr::filter(x$sistec, !!sym("S_NU_CPF") != sistec_invalid) 
  
  x
}

#' @importFrom dplyr %>% 
remove_unliked_cpf <- function(x){
  x$rfept_without_sistec <- dplyr::anti_join(x$rfept, x$sistec,
                                             by = c("R_NU_CPF" = "S_NU_CPF")) %>% 
    dplyr::bind_rows(x$rfept_without_sistec)
  
  x$sistec_without_rfept <- dplyr::anti_join(x$sistec, x$rfept,
                                             by = c("S_NU_CPF" = "R_NU_CPF")) %>% 
    dplyr::bind_rows(x$sistec_without_rfept)
  
  x$rfept <- dplyr::anti_join(x$rfept, x$rfept_without_sistec, by = "R_NU_CPF")
  x$sistec <- dplyr::anti_join(x$sistec, x$sistec_without_rfept, by = "S_NU_CPF")
  
  x
}

#' @importFrom dplyr %>% 
update_unlinked_students <- function(x){
  x$sistec <- dplyr::anti_join(x$sistec, x$sistec_rfept_linked,
                               by = c("S_NU_CPF", "S_NO_CURSO"))
  
  x$rfept <- dplyr::anti_join(x$rfept, x$sistec_rfept_linked, 
                              by = c("R_NU_CPF" = "S_NU_CPF", "R_NO_CURSO"))
  
  x <- remove_unliked_cpf(x)
  x
}

#' @importFrom dplyr %>% sym
separate_wrong_registration <- function(x){

  y <- dplyr::inner_join(x$rfept, x$sistec, by = c("R_NU_CPF" = "S_NU_CPF"))
  y <- y %>% 
    dplyr::group_by(!!sym("R_NO_CURSO"), !!sym("S_NO_CURSO")) %>% 
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") >= 10) %>% 
    dplyr::rename(S_NO_CURSO_LINKED = !!sym("S_NO_CURSO"), S_QT_ALUNOS_LINKED = !!sym("n")) %>% 
    dplyr::inner_join(y, by = "R_NO_CURSO") %>% 
    dplyr::filter(!!sym("S_NO_CURSO_LINKED") == !!sym("S_NO_CURSO")) %>% 
    dplyr::ungroup() 

  # update *_without_*
  x$sistec <- dplyr::anti_join(x$sistec, y, by = c("S_NU_CPF" = "R_NU_CPF", "S_NO_CURSO"))
  x$rfept <- dplyr::anti_join(x$rfept, y, by = c("R_NU_CPF", "R_NO_CURSO"))
  
  x$sistec_without_rfept <-  x$sistec %>% 
    dplyr::bind_rows(x$sistec_without_rfept)
  
  x$rfept_without_sistec <-  x$rfept %>% 
    dplyr::bind_rows(x$rfept_without_sistec)
  
  # update wrong registration
  x$rfept_wrong_beginning <- y %>%
    dplyr::filter(!!sym("S_DT_INICIO_CURSO") != !!sym("R_DT_INICIO_CURSO"))
  
  class(x$rfept_wrong_beginning) <- c("wrong_registration_data_frame", class(x$rfept_complete)[-1])
  
  x$rfept_wrong_cyclo <- y %>% 
    dplyr::filter(!!sym("S_DT_INICIO_CURSO") == !!sym("R_DT_INICIO_CURSO"))
  
  class(x$rfept_wrong_cyclo) <- c("wrong_registration_data_frame", class(x$rfept_complete)[-1])
  
  x
}

#' @importFrom dplyr %>% syms
create_linked_courses_data_frame <- function(x){
  
  select_vars <- c("R_DT_INICIO_CURSO", "R_NO_CURSO", "R_NO_CAMPUS",
                   "S_NO_CURSO_LINKED","S_CO_CICLO_MATRICULA")
  arrange_vars <- c("R_NO_CURSO", "R_DT_INICIO_CURSO")
  
  x$linked_courses <- x$sistec_rfept_linked %>% 
    dplyr::select(!!!syms(select_vars)) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(!!!syms(arrange_vars))
  
  class(x$linked_courses) <- c("linked_courses_data_frame", class(x$rfept_complete)[-1])
  
  x
}

#' @importFrom dplyr %>% sym
split_situation <- function(x){
  
  x$situation_updated <- x$sistec_rfept_linked %>% 
    dplyr::filter(!!sym("S_NO_STATUS_IGUAL") == TRUE)
  
  x$situation_to_update <- x$sistec_rfept_linked %>% 
    dplyr::filter(!!sym("S_NO_STATUS_IGUAL") == FALSE)

  x
}

clean_memory <- function(x){
  # there is no reason to use this table because 
  # the situation were already divided
  x$sistec_rfept_linked <- NULL # clean memory
  
  # there is no reason for use this tables because 
  # the datasets were merged
  x$sistec <- NULL # clean memory
  x$rfept <-NULL # clean memory
  
  x
}
