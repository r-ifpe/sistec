rename_comparison_list <- function(x){

  x$sistec_complete <- NULL
  x$sistec_without_cpf <- rename_sistec_data_frame(x$sistec_without_cpf)
  x$sistec_without_rfept <- rename_sistec_data_frame(x$sistec_without_rfept)
  x$rfept_complete <- rename_rfept_comnplete_data_frame(x$rfept_complete)
  x$rfept_without_cpf <- rename_rfept_data_frame(x$rfept_without_cpf)
  x$rfept_without_sistec <- rename_rfept_data_frame(x$rfept_without_sistec)
  x$rfept_wrong_beginning <- rename_wrong_beginning_data_frame(x$rfept_wrong_beginning)
  x$rfept_wrong_cyclo <- rename_wrong_cyclo_data_frame(x$rfept_wrong_cyclo)
  x$situation_updated <- rename_comparison_data_frame(x$situation_updated)
  x$situation_to_update <- rename_comparison_data_frame(x$situation_to_update)
  x$linked_courses <- rename_linked_course_data_frame(x$linked_courses)
  
  x
}

#' @importFrom dplyr %>% sym
rename_sistec_data_frame <- function(x){
  x %>% 
    dplyr::select(NOME = !!sym("S_NO_ALUNO"),
                  CPF = !!sym("S_NU_CPF"),
                  CICLO = !!sym("S_CO_CICLO_MATRICULA"),
                  CAMPUS = !!sym("S_NO_CAMPUS"),
                  CURSO = !!sym("S_NO_CURSO"))
}

#' @importFrom dplyr %>% sym
rename_rfept_data_frame <- function(x){
  x %>% 
    dplyr::select(NOME = !!sym("R_NO_ALUNO"),
                  CPF = !!sym("R_NU_CPF"),
                  MATRICULA = !!sym("R_CO_MATRICULA"),
                  CAMPUS = !!sym("R_NO_CAMPUS"),
                  CURSO = !!sym("R_NO_CURSO"))
}

#' @importFrom dplyr %>% sym
rename_rfept_comnplete_data_frame <- function(x){
  x %>% 
    dplyr::select(NOME = !!sym("R_NO_ALUNO"),
                  CPF = !!sym("R_NU_CPF"),
                  MATRICULA = !!sym("R_CO_MATRICULA"),
                  CICLO = !!sym("R_DT_INICIO_CURSO"),
                  COTA = !!sym("R_NO_COTA"),
                  CAMPUS = !!sym("R_NO_CAMPUS"),
                  CURSO = !!sym("R_NO_CURSO"))
}

#' @importFrom dplyr %>% sym
#' @importFrom rlang :=
rename_comparison_data_frame <- function(x){
  
  rfept_table <- rfept_table(x)
  
  rfept_matricula <- paste0("MATRICULA_", stringr::str_to_upper(rfept_table))
  rfept_status <- paste0("STATUS_", stringr::str_to_upper(rfept_table))
  
  x %>% 
    dplyr::select(NOME = !!sym("S_NO_ALUNO"),
                  CPF = !!sym("S_NU_CPF"),
                  !!rfept_matricula := !!sym("R_CO_MATRICULA"),
                  !!rfept_status := !!sym("R_NO_STATUS_MATRICULA"),
                  STATUS_SISTEC = !!sym("S_NO_STATUS_MATRICULA"),
                  CAMPUS = !!sym("R_NO_CAMPUS"),
                  CURSO = !!sym("R_NO_CURSO"), # to use rfept is better to update
                  CICLO = !!sym("S_CO_CICLO_MATRICULA")) 
}

#' @importFrom dplyr %>% sym
#' @importFrom rlang :=
rename_wrong_beginning_data_frame <- function(x){

  rfept_table <- rfept_table(x)
  
  rfept_matricula <- paste0("MATRICULA_", stringr::str_to_upper(rfept_table))
  rfept_beginning <- paste0("INICIO_", stringr::str_to_upper(rfept_table))
  
  x %>% 
    dplyr::select(NOME = !!sym("R_NO_ALUNO"),
                  CPF = !!sym("R_NU_CPF"),
                  !!rfept_matricula := !!sym("R_CO_MATRICULA"),
                  SISTEC_CURSO = !!sym("S_NO_CURSO"),
                  SISTEC_CICLO = !!sym("S_CO_CICLO_MATRICULA"),
                  !!rfept_beginning := !!sym("R_DT_INICIO_CURSO"),
                  INICIO_SISTEC = !!sym("S_DT_INICIO_CURSO"),
                  CAMPUS = !!sym("R_NO_CAMPUS"),
                  CURSO = !!sym("R_NO_CURSO")) # to use rfept is better to update
}

#' @importFrom dplyr %>% sym
#' @importFrom rlang :=
rename_wrong_cyclo_data_frame <- function(x){

  rfept_table <- rfept_table(x)
  
  rfept_matricula <- paste0("MATRICULA_", stringr::str_to_upper(rfept_table))

  x %>% 
    dplyr::select(NOME = !!sym("R_NO_ALUNO"),
                  CPF = !!sym("R_NU_CPF"),
                  !!rfept_matricula := !!sym("R_CO_MATRICULA"),
                  SISTEC_CURSO = !!sym("S_NO_CURSO"),
                  SISTEC_CICLO = !!sym("S_CO_CICLO_MATRICULA"),
                  CAMPUS = !!sym("R_NO_CAMPUS"),
                  CURSO = !!sym("R_NO_CURSO")) # to use rfept is better to update
}

#' @importFrom dplyr %>% sym
rename_linked_course_data_frame <- function(x){
  
  rfept_table <- rfept_table(x)
  rfept_course <- paste0("CURSO_", stringr::str_to_upper(rfept_table))
  
  linked_courses <- x %>% 
    dplyr::select(INICIO = !!sym("R_DT_INICIO_CURSO"),
                  CICLO = !!sym("S_CO_CICLO_MATRICULA"),
                  CURSO_SISTEC = !!sym("S_NO_CURSO_LINKED"),
                  !!rfept_course := !!sym("R_NO_CURSO"),
                  CAMPUS = !!sym("R_NO_CAMPUS")) 
}
