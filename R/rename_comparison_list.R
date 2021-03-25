rename_comparison_list <- function(x){

  x$sistec_complete <- NULL
  x$sistec_without_cpf <- rename_sistec_data_frame(x$sistec_without_cpf)
  x$sistec_without_rfept <- rename_sistec_data_frame(x$sistec_without_rfept)
  x$sistec_wrong_cpf <- rename_sistec_data_frame(x$sistec_wrong_cpf)
  x$sistec_duplicated_registry <- rename_sistec_data_frame(x$sistec_duplicated_registry)
  x$sistec_unlinked_entry <- rename_sistec_data_frame(x$sistec_unlinked_entry)
  x$sistec_pending <- rename_sistec_data_frame(x$sistec_pending)
  x$rfept_complete <- rename_rfept_complete_data_frame(x$rfept_complete)
  x$rfept_without_cpf <- rename_rfept_data_frame(x$rfept_without_cpf)
  x$rfept_without_sistec <- rename_rfept_data_frame(x$rfept_without_sistec)
  x$rfept_wrong_cpf <- rename_rfept_data_frame(x$rfept_wrong_cpf)
  x$rfept_duplicated_registry <- rename_rfept_data_frame(x$rfept_duplicated_registry)
  x$rfept_unlinked_entry <- rename_rfept_data_frame(x$rfept_unlinked_entry)
  x$rfept_pending <- rename_rfept_data_frame(x$rfept_pending)
  x$wrong_beginning <- rename_wrong_beginning_data_frame(x$wrong_beginning)
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
                  INICIO = !!sym("S_DT_INICIO_CURSO"),
                  STATUS = !!sym("S_NO_STATUS_MATRICULA"),
                  CAMPUS = !!sym("S_NO_CAMPUS"),
                  CURSO = !!sym("S_NO_CURSO"))
}

#' @importFrom dplyr %>% sym
rename_rfept_data_frame <- function(x){
  x %>% 
    dplyr::select(NOME = !!sym("R_NO_ALUNO"),
                  CPF = !!sym("R_NU_CPF"),
                  MATRICULA = !!sym("R_CO_MATRICULA"),
                  INICIO = !!sym("R_DT_INICIO_CURSO"),
                  STATUS =!!sym("R_NO_STATUS_MATRICULA"),
                  CAMPUS = !!sym("R_NO_CAMPUS"),
                  CURSO = !!sym("R_NO_CURSO"))
}

#' @importFrom dplyr %>% sym
rename_rfept_complete_data_frame <- function(x){
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
  rfept_matricula <- rfept_rename_column(rfept_table, "MATRICULA")
  rfept_status <- rfept_rename_column(rfept_table, "STATUS")
  
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
  rfept_matricula <- rfept_rename_column(rfept_table, "MATRICULA")
  rfept_beginning <- rfept_rename_column(rfept_table, "INICIO")
  
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
  rfept_matricula <- rfept_rename_column(rfept_table, "MATRICULA")

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
  rfept_course <- rfept_rename_column(rfept_table, "CURSO")
  
  linked_courses <- x %>% 
    dplyr::select(INICIO = !!sym("R_DT_INICIO_CURSO"),
                  CICLO = !!sym("S_CO_CICLO_MATRICULA"),
                  CURSO_SISTEC = !!sym("S_NO_CURSO_LINKED"),
                  !!rfept_course := !!sym("R_NO_CURSO"),
                  CAMPUS = !!sym("R_NO_CAMPUS")) 
}

rfept_rename_column <- function(x, column){
  paste0(column, "_", stringr::str_to_upper(x)) 
}

rfept_table <- function(x){
  pos <- stringr::str_which(class(x), "_table")
  table <- sub("_table", "", class(x)[pos])
  if(table == "generic_rfept") {
    table <- "rfept"}
  stringr::str_to_title(table)
}
