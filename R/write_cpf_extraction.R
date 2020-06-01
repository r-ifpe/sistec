write_cpf_extraction <- function(x, path, folder, file){

  if (nrow(x) == 0) return(NULL)
  
  cpf_extraction <- x %>% 
    dplyr::select(NOME = !!sym("R_NO_ALUNO"),
                  CPF = !!sym("R_NU_CPF"),
                  MATRICULA = !!sym("R_CO_MATRICULA"),
                  CAMPUS = !!sym("R_NO_CAMPUS"),
                  CURSO = !!sym("R_NO_CURSO"),
                  INICIO = !!sym("R_DT_INICIO_CURSO"),
                  COTA = !!sym("R_NO_COTA"))
  
  

  path_to_save <- paste0(path,"/", folder)
  grid_vars <- c("CAMPUS", "CURSO", "INICIO", "COTA")
  cpf_extraction_grid <- make_group_grid(cpf_extraction, grid_vars)
  save_cpf_extraction(cpf_extraction, cpf_extraction_grid,
                      path_to_save, file)
  
}


save_cpf_extraction <- function(x, grid, path, file){

  grid_vars <- names(grid)
  qtd_vars <- length(names(x)) - length(grid_vars)
  vars <- names(x)[1:qtd_vars]
  
  # paths to save
  paths <- apply(grid, 1, function(e){
    paste0(path, "/", paste0(e, collapse = "/"))}) 
  
  # creating directories
  invisible(lapply(paths, function(e){
    dir.create(e, recursive = TRUE, showWarnings = FALSE)}))
  
  # expression to use in dplyr::filter
  a <- paste0(grid_vars, " == '")
  filter_exprs <- apply(grid, 1, function(e){
    paste0(paste0(a, e, "'"), collapse = " & ")})
  
  # write result in txt
  invisible(lapply(1:length(filter_exprs), function(e){
    write(x %>% 
            dplyr::filter(eval(parse(text = filter_exprs[e]))) %>% 
            dplyr::select(!!sym("CPF")) %>%
            dplyr::mutate(CPF = paste0(!!sym("CPF"), ";")) %>% 
            dplyr::pull(),
          paste0(paths[e], "/", file, ".txt"),
          sep = "/n")
  }))
}