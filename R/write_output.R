#' Save the comparison results
#'
#' You can use this function to save the results separated by campus. The results will be
#' saved in .xlsx format.
#'
#' @param x A list returned by `compare_sistec()`.
#' @param output_path The folder where you want to save the results.
#' @param output_folder_name The folder's name you want to save the results.
#' 
#' @return None.
#' 
#' @export
write_output <- function(x,
                         output_path = NULL, 
                         output_folder_name = "ARIA"){
  UseMethod("write_output")
}

#' @export
write_output.comparison_list <- function(x, 
                                         output_path = NULL, 
                                         output_folder_name = "ARIA"){
  
  # write results
  if(!is.null(output_path)) {
    if(dir.exists(paste0(output_path,"/ARIA"))){
      unlink(paste0(output_path,"/ARIA"), recursive = TRUE)
    }
    
    rfept_table <- rfept_table(x$rfept_complete)
    path <- paste0(output_path, "/", output_folder_name)

    x <- rename_comparison_list(x)
    group_sistec(x) %>% write_sistec(path, rfept_table)
    group_rfept(x) %>% write_rfept(path, rfept_table)
    group_cpf_registration(x) %>% write_cpf_registration(path)

    write_tables(x, path, rfept_table)
    write_linked_courses(x, path)
  } else {
    stop("Please, select a folder to download the results.")
  }
}

#' @importFrom dplyr %>% sym
group_sistec <- function(x){
  
  sistec_without_cpf <- x$sistec_without_cpf %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "sistec_without_cpf")
  
  sistec_without_rfept <- x$sistec_without_rfept %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "sistec_without_rfept")
  
  sistec_wrong_cpf <- x$sistec_wrong_cpf %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "sistec_wrong_cpf")
  
  sistec_duplicated_registry <- x$sistec_duplicated_registry %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "sistec_duplicated_registry")
  
  sistec_unlinked_entry <- x$sistec_unlinked_entry %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "sistec_unlinked_entry")
  
  sistec_pending <- x$sistec_pending %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "sistec_pending")
  
  dplyr::full_join(sistec_without_cpf, sistec_without_rfept, by = c("CAMPUS", "CURSO")) %>%
    dplyr::full_join(sistec_wrong_cpf, by = c("CAMPUS", "CURSO")) %>% 
    dplyr::full_join(sistec_duplicated_registry, by = c("CAMPUS", "CURSO")) %>% 
    dplyr::full_join(sistec_unlinked_entry, by = c("CAMPUS", "CURSO")) %>% 
    dplyr::full_join(sistec_pending, by = c("CAMPUS", "CURSO"))
}

#' @importFrom dplyr %>% sym
group_rfept <- function(x){

  rfept_without_cpf <- x$rfept_without_cpf %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "rfept_without_cpf")
  
  rfept_without_sistec <- x$rfept_without_sistec %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "rfept_without_sistec")
  
  rfept_wrong_cpf <- x$rfept_wrong_cpf %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "rfept_wrong_cpf")
  
  rfept_duplicated_registry <- x$rfept_duplicated_registry %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "rfept_duplicated_registry")
  
  rfept_unlinked_entry <- x$rfept_unlinked_entry %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "rfept_unlinked_entry")
  
  rfept_pending <- x$rfept_pending %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "rfept_pending")

  wrong_beginning <- x$wrong_beginning %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "wrong_beginning")
  
  situation_to_update <- x$situation_to_update %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "situation_to_update")
  
  situation_updated <- x$situation_updated %>%
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), .key = "situation_updated")  
 
  dplyr::full_join(rfept_without_cpf, rfept_without_sistec, by = c("CAMPUS", "CURSO")) %>%
    dplyr::full_join(rfept_wrong_cpf, by = c("CAMPUS", "CURSO")) %>% 
    dplyr::full_join(rfept_duplicated_registry, by = c("CAMPUS", "CURSO")) %>% 
    dplyr::full_join(rfept_unlinked_entry, by = c("CAMPUS", "CURSO")) %>% 
    dplyr::full_join(rfept_pending, by = c("CAMPUS", "CURSO")) %>% 
    dplyr::full_join(wrong_beginning, by = c("CAMPUS", "CURSO")) %>% 
    dplyr::full_join(situation_to_update, by = c("CAMPUS", "CURSO")) %>% 
    dplyr::full_join(situation_updated, by = c("CAMPUS", "CURSO"))
}

#' @importFrom dplyr %>% sym
group_cpf_registration <- function(x){
  dplyr::semi_join(x$rfept_complete, x$rfept_without_sistec, by = "MATRICULA") %>% 
    dplyr::group_nest(!!sym("CAMPUS"), !!sym("CURSO"), !!sym("CICLO"), .key = "quota")
}

write_sistec <- function(x, path, rfept_table){
  
  if(nrow(x) == 0) return()
  
  invisible(
    lapply(1:nrow(x), function(e){
      path_to_save <- paste0(path, "/Retificar no Sistec/",  x$CAMPUS[e], "/", x$CURSO[e])
      dir.create(path_to_save, recursive = TRUE)
      sistec <- list()
      sistec[["Sem CPF"]] <- x$sistec_without_cpf[e][[1]]
      sistec[["Erro no CPF"]] <- x$sistec_wrong_cpf[e][[1]]
      sistec[["Entrada duplicada"]] <- x$sistec_duplicated_registry[e][[1]]
      sistec[[paste0("Inserir no ", rfept_table)]] <- x$sistec_without_rfept[e][[1]]
      sistec[["Entrada n\u00e3o encontrada"]] <- x$sistec_unlinked_entry[e][[1]]
      sistec[["Inspe\u00e7\u00e3o manual"]] <- x$sistec_pending[e][[1]]
      openxlsx::write.xlsx(sistec, paste0(path_to_save, "/Sistec.xlsx"))
    })
  )
}

write_rfept <- function(x, path, rfept_table){
  
  if(nrow(x) == 0) return()
  
  invisible(
    lapply(1:nrow(x), function(e){
      path_to_save <- paste0(path, "/Retificar no ", rfept_table, "/",  x$CAMPUS[e], "/", x$CURSO[e])
      dir.create(path_to_save, recursive = TRUE)
      rfept <- list()
      rfept[["Sem CPF"]] <- x$rfept_without_cpf[e][[1]]
      rfept[["Erro no CPF"]] <- x$rfept_wrong_cpf[e][[1]]
      rfept[["Entrada duplicada"]] <- x$rfept_duplicated_registry[e][[1]]
      rfept[["Inserir no Sistec"]] <- x$rfept_without_sistec[e][[1]]
      rfept[["Entrada n\u00e3o encontrada"]] <- x$rfept_unlinked_entry[e][[1]]
      rfept[["Inspe\u00e7\u00e3o manual"]] <- x$rfept_pending[e][[1]]
      rfept[["Retificar Inicio"]] <- x$wrong_beginning[e][[1]]
      rfept[["Retificar Situa\u00e7\u00e3o"]] <- x$situation_to_update[e][[1]]
      rfept[["Situa\u00e7\u00f5es Atualizadas"]] <- x$situation_updated[e][[1]]
      openxlsx::write.xlsx(rfept, paste0(path_to_save, "/", 
                                         rfept_table, ".xlsx"))
    })
  )
}

#' @importFrom dplyr sym
write_cpf_registration <- function(x, path){

  if(nrow(x) == 0) return()
  
  invisible(
    lapply(1:nrow(x), function(e){

      path_to_save <- paste0(path, "/Cadastrar Alunos/",
                             x$CAMPUS[e], "/", x$CURSO[e], "/", x$CICLO[e])
      dir.create(path_to_save, recursive = TRUE)
      
      quotas_table <- dplyr::arrange(x$quota[e][[1]], !!sym("COTA"))
      unique_quotas <- unique(quotas_table$COTA)
      
      cpfs <- list()
      cpfs[["Inserir no Sistec"]] <- quotas_table
      
      for(i in 1:length(unique_quotas)){
        cpf_quota <- quotas_table[quotas_table$COTA == unique_quotas[i], ]$CPF
        cpf_quota <- paste0(cpf_quota, ";")
        cpfs[[unique_quotas[i]]] <- cpf_quota
      }

      openxlsx::write.xlsx(cpfs, paste0(path_to_save, "/Cadastrar alunos.xlsx"))
    })
  )
}

write_tables <- function(x, path, rfept_table){

  path_to_save <- paste0(path, "/Tabelas Utilizadas")
  dir.create(path_to_save, recursive = TRUE)
  
  tables <- list()
  tables[["Sistec sem CPF"]] <- x$sistec_without_cpf
  tables[[paste0("Sistec sem ", rfept_table)]] <- x$sistec_without_rfept
  tables[["Sistec com CPF Repetidos"]] <- x$sistec_wrong_cpf
  tables[["Sistec com Entrada Repetida"]] <- x$sistec_duplicated_registry
  tables[["Sistec com entrada n\u00e3o encontrada"]] <- x$sistec_unlinked_entry
  tables[["Sistec Inspe\u00e7\u00e3o Manual"]] <- x$sistec_pending
  
  tables[[paste0(rfept_table, " sem CPF")]] <- x$rfept_without_cpf
  tables[[paste0(rfept_table, " sem Sistec")]] <- x$rfept_without_sistec
  tables[[paste0(rfept_table, " com CPF Repetidos")]] <- x$rfept_wrong_cpf
  tables[[paste0(rfept_table, " com Entrada Repetida")]] <- x$rfept_duplicated_registry
  tables[[paste0(rfept_table, " com entrada n\u00e3o encontrada")]] <- x$rfept_unlinked_entry
  tables[[paste0(rfept_table, " Inspe\u00e7\u00e3o Manual")]] <- x$rfept_pending
  
  tables[["Data de In\u00edcio Errada"]] <- x$rfept_wrong_beginning
  tables[["Situa\u00e7\u00f5es Atualizadas"]] <- x$situation_updated
  tables[["Situa\u00e7\u00f5es Desatualizadas"]] <- x$situation_to_update
  tables[["Cursos Relacionados"]] <- x$linked_courses
  
  openxlsx::write.xlsx(tables, paste0(path_to_save, "/Tabelas Utilizadas.xlsx"))
}

write_linked_courses <- function(x, path){
  
  path_to_save <- paste0(path, "/Cursos Relacionados")
  dir.create(path_to_save, recursive = TRUE)
  
  tables <- list()
  tables[["Cursos Relacionados"]] <- x$linked_courses
  
  openxlsx::write.xlsx(tables, paste0(path_to_save, "/Cursos relacionados.xlsx"))
}
