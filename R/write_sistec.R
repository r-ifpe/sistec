#' @importFrom rlang sym syms
#' @importFrom dplyr %>% 
write_sistec <- function(sistec, path, folder, file){
  
  path <- paste0(path,"/", folder)

  campus <- sistec$NO_CAMPUS %>% unique()
  
  for(i in 1:length(campus)){
    
    sistec_campus <- sistec %>% 
      dplyr::filter(NO_CAMPUS == campus[i])
    
    if(nrow(sistec_campus) == 0) next()
    
    cursos <- sistec_campus$NO_CURSO %>% unique()
    cursos <- gsub("/|:|\"", "_", cursos)
    
    for(j in 1:length(cursos)){
      
      sistec_campus_curso <- sistec_campus %>% 
        dplyr::filter(NO_CURSO == cursos[j])
      
      if(nrow(sistec_campus_curso) == 0) next()
      
      path_campus_curso <- paste0(path,"/", campus[i], "/", cursos[j])
      dir.create(path_campus_curso, recursive = TRUE)
      
      vars <- c("NO_ALUNO",	"NU_CPF",	"CO_CICLO_MATRICULA", "NO_STATUS_MATRICULA")

      openxlsx::write.xlsx(sistec_campus_curso %>% dplyr::select(!!!syms(vars)),
                           paste0(path_campus_curso, "/",
                                  file, ".xlsx")) 
      
    }
  }
}