write_comparison <- function(x, path){
  
  rfept_table <- rfept_table(x$rfept_complete)

  write_status_comparison(x$situation_to_update, rfept_table, 
                          path, subfolder = TRUE, 
                          "Retificar Situa\u00e7\u00e3o",
                          "alterar situa\u00e7\u00e3o")
  # write divided data
  write_sistec(x$sistec_without_cpf, path, subfolder = TRUE,
               "Retificar CPF/Sistec", "sem cpf")
  write_sistec(x$sistec_without_rfept, path, subfolder = TRUE,
               paste("Inserir no", rfept_table), 
               paste("sem", stringr::str_to_lower(rfept_table)))
  
  write_rfept(x$rfept_without_cpf, path, subfolder = TRUE,
              paste0("Retificar CPF/", rfept_table), "sem cpf")
  write_rfept(x$rfept_without_sistec, path, subfolder = TRUE,
              "Inserir no Sistec", "sem sistec")
  write_cpf_extraction(x$rfept_without_sistec, path,
                       "Cadastrar em Lote", "cpfs")

  
  write_status_comparison(x$situation_to_update, rfept_table, 
                          path, subfolder = TRUE, 
                          "Retificar Situa\u00e7\u00e3o",
                          "alterar situa\u00e7\u00e3o")
  
  # write completed data
  write_sistec(x$sistec_without_cpf, path, subfolder = FALSE,
               "Tabelas Utilizadas", file = "Sistec sem CPF")
  write_sistec(x$sistec_without_rfept, path, subfolder = FALSE,
               "Tabelas Utilizadas", file = paste0("Sistec sem ", rfept_table))
  
  write_rfept(x$rfept_without_cpf, path, subfolder = FALSE,
              "Tabelas Utilizadas", file = paste0(rfept_table, " sem CPF"))
  write_rfept(x$rfept_without_sistec, path, subfolder = FALSE,
              "Tabelas Utilizadas", file = paste0(rfept_table, " sem Sistec"))
  
  write_status_comparison(x$situation_updated, rfept_table, path, subfolder = FALSE,
                          "Tabelas Utilizadas", file = "Situa\u00e7\u00f5es atualizadas")
  write_status_comparison(x$situation_to_update, rfept_table, path, subfolder = FALSE,
                          "Tabelas Utilizadas", file = "Situa\u00e7\u00f5es desatualizadas")
  
  write_linked_courses(x$linked_courses, rfept_table, path, 
                       "Tabelas Utilizadas", file = "Cursos relacionados")
} 
