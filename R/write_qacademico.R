#' @importFrom rlang sym syms
write_qacademico <- function(qacademico, path, folder, file){
  
  path <- paste0(path,"/", folder)
  
  campus <- qacademico$Campus %>% unique()

  for(i in 1:length(campus)){
    
    qacademico_campus <- qacademico %>% 
      dplyr::filter(!!sym("Campus") == campus[i])
    
    if(nrow(qacademico_campus) == 0) next()
    
    cursos <- qacademico_campus$Curso %>% unique()
    cursos <- gsub("/|:| \\.", "_", cursos)
    
    for(j in 1:length(cursos)){
      
      qacademico_campus_curso <- qacademico_campus %>% 
        dplyr::filter(!!sym("Curso") == cursos[j])
      
      if(nrow(qacademico_campus_curso) == 0) next()
      
      path_campus_curso <- paste0(path,"/", campus[i], "/", cursos[j])
      dir.create(path_campus_curso, recursive = TRUE)
      
      vars <- c("Matr\u00edcula", "Cpf", "Nome", 
                "Situa\u00e7\u00e3o.Matr\u00edcula")

      openxlsx::write.xlsx(qacademico_campus_curso %>% dplyr::select(!!!syms(vars)),
                           paste0(path_campus_curso, "/",
                                  file, ".xlsx")) 
      
    }
  }
}