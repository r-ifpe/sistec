merge_sistec_rfept <- function(x){

  x$sistec_rfept_linked <- dplyr::inner_join(x$sistec, x$rfept,
                                             by = c("S_NU_CPF" = "R_NU_CPF")) %>% 
    link_courses() %>% 
    link_ciclos() 
  
  # %>% complete_campus()
  
  x
}

#' @importFrom dplyr %>% sym
link_courses <- function(x){
  # to link a course, they must initiate at the same time and 
  # have the majority of the students point to the same course
  
  x <- x %>% 
    dplyr::filter(!!sym("R_DT_INICIO_CURSO") == !!sym("S_DT_INICIO_CURSO"))
  
  x %>% 
    dplyr::group_by(!!sym("R_NO_CURSO"), !!sym("S_NO_CURSO")) %>% 
    dplyr::tally() %>% 
    dplyr::filter(!!sym("n") >= 10) %>% 
    dplyr::rename(S_NO_CURSO_LINKED = !!sym("S_NO_CURSO"), S_QT_ALUNOS_LINKED = !!sym("n")) %>% 
    dplyr::inner_join(x, by = "R_NO_CURSO") %>% 
    dplyr::filter(!!sym("S_NO_CURSO_LINKED") == !!sym("S_NO_CURSO")) %>% 
    dplyr::ungroup()
}

#' @importFrom dplyr %>% sym
link_ciclos <- function(x){
  # sometimes a same ciclo can point to different courses in rfept, choose
  # that ciclo with the majority of students

  ciclos <- x %>%
    dplyr::group_by(!!sym("S_CO_CICLO_MATRICULA"), !!sym("S_QT_ALUNOS_LINKED")) %>% 
    dplyr::tally() %>% 
    dplyr::arrange(!!sym("S_CO_CICLO_MATRICULA") , dplyr::desc(!!sym("n"))) %>% 
    dplyr::distinct(!!sym("S_CO_CICLO_MATRICULA"), .keep_all = TRUE)
  
  dplyr::semi_join(x, ciclos, by = c("S_CO_CICLO_MATRICULA", "S_QT_ALUNOS_LINKED"))
}

#' @importFrom dplyr sym
complete_campus <- function(x){
  # some student registraion doesn't have information about campus
  # we complete this part using information in sistec
  dplyr::mutate(x, R_NO_CAMPUS = ifelse(!!sym("R_NO_CAMPUS") == "SEM CAMPUS",
                                        !!sym("S_NO_CAMPUS"),
                                        !!sym("R_NO_CAMPUS")))
}
