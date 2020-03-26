
# read files to copy
a <- sistec::read_qacademico("C:/Pesquisa/dados/sistec_app/arquivos/")
b <- sistec::read_sistec("C:/Pesquisa/dados/sistec_app/arquivos/")
t <- full_join(a,b, by = c("Cpf" = "NU_CPF"))

# generate random cpf and names 
cpfs <- unique(t$Cpf)
cpfs_rnd <- NULL
nome_rnd <- NULL



for (i in 1:length(cpfs)){
  cpf_rnd <- stringi::stri_rand_strings(1,11, pattern = "[0-9]") %>% 
    stringr::str_replace(pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
                         replacement = "\\1.\\2.\\3-")
  
  cpfs_rnd[i] <- cpf_rnd
  
  
  str <- stri_rand_strings(3, 6, "[a-zA-Z]")
  nome_rnd[i] <- paste(str[1], str[2], str[3])
}


cpfs_table <- data.frame(cpfs, cpfs_rnd, nome_rnd, stringsAsFactors = FALSE)
t_cpf_rnd <- full_join(t, cpfs_table, by = c("Cpf" = "cpfs"))

for (i in 1:nrow(t)){
  if(!is.na(t_cpf_rnd$Nome[i])) {
    t_cpf_rnd$Nome[i] <- t_cpf_rnd$nome_rnd[i]
  }
  
  if(!is.na(t_cpf_rnd$NO_ALUNO[i])) {
    t_cpf_rnd$NO_ALUNO[i] <- t_cpf_rnd$nome_rnd[i]
  }
}

# qacademico files
qacademico <- t_cpf_rnd %>% 
  transmute(Nome, `Situação Matrícula` = Situação.Matrícula, Cpf = .data$cpfs_rnd) %>% 
  distinct()


qacademico[1:500,] %>% 
  openxlsx::write.xlsx("fake_data_qacademico_1.xlsx")

qacademico[501:931,] %>% # NA starts at 932
  openxlsx::write.xlsx("fake_data_qacademico_2.xlsx")

# sistec files

sistec <- t_cpf_rnd %>% 
  transmute(NO_ALUNO, CO_CICLO_MATRICULA, NO_STATUS_MATRICULA, NU_CPF = .data$cpfs_rnd) %>% 
  distinct() %>% 
  mutate(NU_CPF = stringr::str_remove_all(NU_CPF, "[.-]")) %>% 
  mutate(NU_CPF = as.numeric(NU_CPF)) 

sistec[1:500,] %>%  
  write.table("fake_data_sistec_1.csv", row.names = FALSE, sep = ";")

sistec[501:1019,] %>% 
  write.table("fake_data_sistec_2.csv", row.names = FALSE, sep = ";")  


