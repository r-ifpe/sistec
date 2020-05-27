#' @importFrom dplyr %>% sym
write_sistec <- function(x, path, subfolder, folder, file){
  
  path <- paste0(path,"/", folder)
  
  sistec <- x %>% 
    dplyr::select(NOME = !!sym("S_NO_ALUNO"),
                  CPF = !!sym("S_NU_CPF"),
                  CICLO = !!sym("S_CO_CICLO_MATRICULA"),
                  CAMPUS = !!sym("S_NO_CAMPUS"),
                  CURSO = !!sym("S_NO_CURSO"))
  
  if(subfolder){
    grid_vars <- c("CAMPUS", "CURSO")
    sistec_grid <- make_group_grid(sistec, grid_vars)
    save_results(sistec, sistec_grid, path, file)
  } else{
    save_results(sistec, grid = NULL, path, file)
  }
}

#' @importFrom dplyr %>% sym
write_rfept <- function(x, path, folder, subfolder, file){
  
  path <- paste0(path,"/", folder)
  
  rfept <- x %>% 
    dplyr::select(NOME = !!sym("R_NO_ALUNO"),
                  CPF = !!sym("R_NU_CPF"),
                  MATRICULA = !!sym("R_CO_MATRICULA"),
                  CAMPUS = !!sym("R_NO_CAMPUS"),
                  CURSO = !!sym("R_NO_CURSO"))
  
  if(subfolder){
    grid_vars <- c("CAMPUS", "CURSO")
    rfept_grid <- make_group_grid(rfept, grid_vars)
    save_results(rfept, rfept_grid, path, file)
  } else{
    save_results(rfept, grid = NULL, path, file)
  }
}

#' @importFrom dplyr %>% sym
write_status_comparison <- function(x, table, path, subfolder, folder, file){
  path <- paste0(path,"/", folder)
  rfept_matricula <- paste0("MATRICULA_", stringr::str_to_upper(table))
  rfept_status <- paste0("STATUS_", stringr::str_to_upper(table))
  
  comparison <- x %>% 
    dplyr::select(NOME = !!sym("S_NO_ALUNO"),
                  CPF = !!sym("S_NU_CPF"),
                  !!rfept_matricula := !!sym("R_CO_MATRICULA"),
                  !!rfept_status := !!sym("R_NO_STATUS_MATRICULA"),
                  STATUS_SISTEC = !!sym("S_NO_STATUS_MATRICULA"),
                  CAMPUS = !!sym("R_NO_CAMPUS"),
                  CURSO = !!sym("R_NO_CURSO"), # to use rfept is better to update
                  CICLO = !!sym("S_CO_CICLO_MATRICULA")) 
  
  if(subfolder){
    grid_vars <- c("CAMPUS", "CURSO", "CICLO")
    comparison_grid <- make_group_grid(comparison, grid_vars)
    save_results(comparison, comparison_grid, path, file)
  } else{
    save_results(comparison, grid = NULL, path, file)
  }
}

#' @importFrom dplyr %>% sym
write_linked_courses <- function(x, rfept_table, path, folder, file){
  
  path <- paste0(path,"/", folder)
  rfept_course <- paste0("CURSO_", stringr::str_to_upper(rfept_table))
  
  linked_courses <- x %>% 
    dplyr::select(INICIO = !!sym("R_DT_INICIO_CURSO"),
                  CICLO = !!sym("S_CO_CICLO_MATRICULA"),
                  CURSO_SISTEC = !!sym("S_NO_CURSO_LINKED"),
                  !!rfept_course := !!sym("R_NO_CURSO")) 
  
  save_results(linked_courses, grid = NULL, path, file)
}
