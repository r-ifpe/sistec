create_sistec_rfept_list <- function(sistec, rfept, linked_courses = NULL){
  
  if(is.null(linked_courses)){
    linked_courses <- data.frame()
  } 

  list(sistec = sistec,
       sistec_complete = sistec,
       sistec_without_cpf = data.frame(),
       sistec_without_rfept = data.frame(),
       sistec_wrong_cpf = data.frame(),
       sistec_duplicated_registry = data.frame(),
       sistec_pending = data.frame(),
       rfept = rfept,
       rfept_complete = rfept,
       rfept_without_cpf = data.frame(),
       rfept_without_sistec = data.frame(),
       rfept_wrong_cpf = data.frame(),
       rfept_duplicated_registry = data.frame(),
       rfept_pending = data.frame(),
       wrong_beginning = data.frame(),
       situation_updated = data.frame(),
       situation_to_update = data.frame(),
       sistec_rfept_linked = data.frame(),
       linked_courses = linked_courses)
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

#' @importFrom dplyr %>% sym
remove_duplicated_registry <- function(x){
  sistec_duplicated_registry <- x$sistec %>% 
    dplyr::group_by(!!sym("S_NU_CPF"), !!sym("S_NO_CURSO"), !!sym("S_DT_INICIO_CURSO")) %>% 
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") > 1)
  
  x$sistec_duplicated_registry <- x$sistec %>% 
    dplyr::semi_join(sistec_duplicated_registry, by = c("S_NU_CPF", "S_NO_CURSO", "S_DT_INICIO_CURSO"))
  
  x$sistec <- x$sistec %>% 
    dplyr::anti_join(x$sistec_duplicated_registry, by = c("S_NU_CPF", "S_NO_CURSO", "S_DT_INICIO_CURSO"))
  
  rfept_duplicated_registry <- x$rfept %>% 
    dplyr::group_by(!!sym("R_NU_CPF"), !!sym("R_NO_CURSO"), !!sym("R_DT_INICIO_CURSO")) %>% 
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") > 1)
  
  x$rfept_duplicated_registry <- x$rfept %>% 
    dplyr::semi_join(rfept_duplicated_registry, by = c("R_NU_CPF", "R_NO_CURSO", "R_DT_INICIO_CURSO"))
  
  x$rfept <- x$rfept %>% 
    dplyr::anti_join(x$rfept_duplicated_registry, by = c("R_NU_CPF", "R_NO_CURSO", "R_DT_INICIO_CURSO"))
  
  x
}

#' @importFrom dplyr %>% sym
remove_wrong_cpf <- function(x){
  
  sistec_wrong_cpf <- x$sistec %>%
    dplyr::mutate(S_NO_ALUNO = stringr::str_trim(!!sym("S_NO_ALUNO"), side = "both")) %>%
    dplyr::distinct(!!sym("S_NO_ALUNO"), !!sym("S_NU_CPF")) %>%
    dplyr::group_by(!!sym("S_NU_CPF")) %>%
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") > 1)
  
  x$sistec_wrong_cpf <- x$sistec %>% 
    dplyr::semi_join(sistec_wrong_cpf, by = "S_NU_CPF")
  
  x$sistec <- x$sistec %>% 
    dplyr::anti_join(x$sistec_wrong_cpf, by = "S_NU_CPF")

  rfept_wrong_cpf <- x$rfept %>%
    dplyr::mutate(R_NO_ALUNO = stringr::str_trim(!!sym("R_NO_ALUNO"), side = "both")) %>%
    dplyr::distinct(!!sym("R_NO_ALUNO"), !!sym("R_NU_CPF")) %>%
    dplyr::group_by(!!sym("R_NU_CPF")) %>%
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") > 1)
  
  x$rfept_wrong_cpf <- x$rfept %>% 
    dplyr::semi_join(rfept_wrong_cpf, by = "R_NU_CPF")
  
  x$rfept <- x$rfept %>% 
    dplyr::anti_join(x$rfept_wrong_cpf, by = "R_NU_CPF")
  
  x
}

#' @importFrom dplyr %>% 
remove_unlinked_cpf <- function(x){
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

#' @importFrom dplyr %>% sym
separate_wrong_beginning <- function(x){

  y <- dplyr::inner_join(x$rfept, x$sistec, by = c("R_NU_CPF" = "S_NU_CPF"))
  y <- y %>% 
    dplyr::group_by(!!sym("R_NO_CURSO"), !!sym("S_NO_CURSO")) %>% 
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") >= 8) %>% 
    dplyr::rename(S_NO_CURSO_LINKED = !!sym("S_NO_CURSO"), S_QT_ALUNOS_LINKED = !!sym("n")) %>% 
    dplyr::inner_join(y, by = "R_NO_CURSO") %>% 
    dplyr::filter(!!sym("S_NO_CURSO_LINKED") == !!sym("S_NO_CURSO")) %>% 
    dplyr::filter(!!sym("S_DT_INICIO_CURSO") != !!sym("R_DT_INICIO_CURSO")) %>% 
    dplyr::ungroup() 

  # remove students with more than on entry
  duplicated_students <- y %>%
    dplyr::group_by(!!sym("R_CO_MATRICULA"), !!sym("R_NU_CPF")) %>% 
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") > 1) 
  
  y <- y %>% 
    dplyr::anti_join(duplicated_students, by = "R_NU_CPF")
  
  duplicated_students <- y %>%
    dplyr::group_by(!!sym("S_CO_CICLO_MATRICULA"), !!sym("R_NU_CPF")) %>% 
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") > 1) 
  
  y <- y %>% 
    dplyr::anti_join(duplicated_students, by = "R_NU_CPF")
  
  # update wrong registration
  x$sistec <- dplyr::anti_join(x$sistec, y, by = c("S_NU_CPF" = "R_NU_CPF", "S_NO_CURSO"))
  x$rfept <- dplyr::anti_join(x$rfept, y, by = c("R_NU_CPF", "R_NO_CURSO"))
  
  x$wrong_beginning <- y %>%
    dplyr::filter(!!sym("S_DT_INICIO_CURSO") != !!sym("R_DT_INICIO_CURSO"))
  
  class(x$wrong_beginning) <- c("wrong_registration_data_frame", class(x$rfept_complete)[-1])
  
  # x$rfept_wrong_cyclo <- y %>% 
  #   dplyr::filter(!!sym("S_DT_INICIO_CURSO") == !!sym("R_DT_INICIO_CURSO"))
  # 
  # class(x$rfept_wrong_cyclo) <- c("wrong_registration_data_frame", class(x$rfept_complete)[-1])
  # 
  x
}

#' @importFrom dplyr %>% sym
pending_manual_inspection <- function(x){
  
 x <- remove_unlinked_cpf(x)
 x$sistec_pending <- x$sistec
 x$rfept_pending <- x$rfept
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
