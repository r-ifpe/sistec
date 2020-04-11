#' @importFrom rlang sym syms
#' @importFrom dplyr %>% 
write_status_comparison <- function(x, path, folder, file){

  path <- paste0(path,"/", folder)

  vars <- c("Cpf", "Matricula_q", "Nome_q", "Nome_sistec", 
            "Curso_q", "Curso_sistec", "Status_q", "Status_sistec",
            "Ciclo_sistec", "Campus_q")
  
  qacademico <- x %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(Status == FALSE) %>% 
    dplyr::select(!!!syms(vars))

  campus <- qacademico$Campus_q %>% unique()
  
  for(i in 1:length(campus)){
    
    qacademico_campus <- qacademico %>% 
      dplyr::filter(Campus_q == campus[i])
    
    if(nrow(qacademico_campus) == 0) next()

    cursos <- qacademico_campus$Curso_q %>% unique()
    cursos <- gsub("/|:| \\.", "_", cursos)
    
    for(j in 1:length(cursos)){
     
      qacademico_campus_curso <- qacademico_campus %>% 
        dplyr::filter(Curso_q == cursos[j])
      
      if(nrow(qacademico_campus_curso) == 0) next()
      
      path_campus_curso <- paste0(path,"/", campus[i], "/", cursos[j])
      dir.create(path_campus_curso, recursive = TRUE)
      
      ciclos <- qacademico_campus_curso$Ciclo_sistec %>% unique() 
      
      print_vars <- c("Cpf", "Matricula_q", "Nome_q", "Nome_sistec", 
                      "Curso_q", "Curso_sistec", "Status_q", "Status_sistec")
      
      qacademico_campus_curso <- lapply(1:length(ciclos), function(e, print_vars){
        qacademico_campus_curso %>%
          dplyr::filter(!!sym("Ciclo_sistec") == ciclos[e]) %>%
          dplyr::arrange(!!sym("Status_sistec")) %>% 
          dplyr::select(!!!syms(print_vars))
      }, print_vars)
      
      names(qacademico_campus_curso) <- ciclos
      openxlsx::write.xlsx(qacademico_campus_curso,
                           paste0(path_campus_curso, "/",
                                  file, ".xlsx")) 
    }

  }
}
