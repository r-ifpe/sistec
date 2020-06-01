merge_sistec_rfept <- function(x){

  x <- link_students(x)
  x <- update_unlinked_students(x) 
  x
}

#' @importFrom dplyr %>% 
link_students <- function(x){
  x$sistec_rfept_linked <- dplyr::inner_join(x$sistec, x$rfept,
                                             by = c("S_NU_CPF" = "R_NU_CPF")) %>% 
    link_courses() %>% 
    link_ciclos()
  
  x
}

#' @importFrom dplyr %>% 
#' @importFrom rlang sym
link_courses <- function(x){
  # to link a course, they must initiate at the same time and 
  # have the majority of the students point to the same course
  
  x <- x %>% 
    dplyr::filter(!!sym("R_DT_INICIO_CURSO") == !!sym("S_DT_INICIO_CURSO"))
  
  x %>% 
    dplyr::group_by(!!sym("R_NO_CURSO"), !!sym("S_NO_CURSO")) %>% 
    dplyr::tally() %>% 
    dplyr::arrange(!!sym("R_NO_CURSO"), dplyr::desc(!!sym("n"))) %>% 
    dplyr::distinct(!!sym("R_NO_CURSO"), .keep_all = TRUE) %>% 
    dplyr::rename(S_NO_CURSO_LINKED = !!sym("S_NO_CURSO"), S_QT_ALUNOS_LINKED = !!sym("n")) %>% 
    dplyr::right_join(x, by = "R_NO_CURSO") %>% 
    dplyr::filter(!!sym("S_NO_CURSO") == !!sym("S_NO_CURSO_LINKED")) %>% 
    dplyr::ungroup()
}

#' @importFrom rlang sym
#' @importFrom dplyr %>% 
link_ciclos <- function(x){
  # sometimes a same ciclo can point to different courses in rfept, choose
  # that ciclo with the majority of students
  
  x %>%
    dplyr::group_by(!!sym("S_CO_CICLO_MATRICULA"), !!sym("S_QT_ALUNOS_LINKED")) %>% 
    dplyr::tally() %>% 
    dplyr::arrange(!!sym("S_CO_CICLO_MATRICULA") ,desc(!!sym("n"))) %>% 
    dplyr::distinct(!!sym("S_CO_CICLO_MATRICULA"), .keep_all = TRUE) %>% 
    dplyr::semi_join(x, ., by = c("S_CO_CICLO_MATRICULA", "S_QT_ALUNOS_LINKED"))
}

#' @importFrom dplyr %>% 
update_unlinked_students <- function(x){
  
  x$sistec_without_rfept <- dplyr::anti_join(x$sistec, x$sistec_rfept_linked,
                                             by = c("S_NU_CPF", "S_NO_CURSO")) %>% 
    dplyr::bind_rows(x$sistec_without_rfept)

  x$rfept_without_sistec <- dplyr::anti_join(x$rfept, x$sistec_rfept_linked, 
                                      by = c("R_NU_CPF" = "S_NU_CPF", "R_NO_CURSO")) %>% 
    dplyr::bind_rows(x$rfept_without_sistec)
  
  # there is no reason for use this tables because 
  # the datasets were merged
  x$sistec <- NULL # clean memory
  x$rfept <-NULL # clean memory

  x
}