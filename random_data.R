
# read files to copy
a <- sistec::read_qacademico("C:/Pesquisa/dados/sistec_app/arquivos/")
b <- sistec::read_sistec("C:/Pesquisa/dados/sistec_app/arquivos/")



# x <- sistec::read_qacademico("C:/Pesquisa/sistec/tests/")
# y <- sistec::read_sistec("C:/Pesquisa/sistec/tests/")
# t1 <- full_join(x, y, by = c("Cpf" = "NU_CPF"))
# generate random cpf and names 

cpfs_rnd <- NULL
nome_rnd <- NULL
cpfs <- c(a$Cpf, b$NU_CPF) %>% unique()


for (i in 1:length(cpfs)){
  cpf_rnd <- stringi::stri_rand_strings(1,11, pattern = "[0-9]") %>% 
    stringr::str_replace(pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
                         replacement = "\\1.\\2.\\3-")
  
  cpfs_rnd[i] <- cpf_rnd
  
  
  str <- stri_rand_strings(3, 6, "[a-zA-Z]")
  nome_rnd[i] <- paste(str[1], str[2], str[3])
}

cpfs_table <- data.frame(cpfs, cpfs_rnd, nome_rnd, stringsAsFactors = FALSE)

#### another approuch


# qacademico
qacademico <- a %>% 
  left_join(cpfs_table, by = c("Cpf" = "cpfs")) %>% 
  transmute(Nome = nome_rnd, `Situação.Matrícula`, Cpf = cpfs_rnd)

qacademico[1:500,] %>% 
  openxlsx::write.xlsx("fake_data_qacademico_1.xlsx")

qacademico[501:936,] %>% 
  openxlsx::write.xlsx("fake_data_qacademico_2.xlsx")

# sistec 
sistec <- b %>% 
  left_join(cpfs_table, by = c("NU_CPF" = "cpfs")) %>% 
  transmute(NO_ALUNO = nome_rnd, NU_CPF = cpfs_rnd, 
            CO_CICLO_MATRICULA, NO_STATUS_MATRICULA) %>% 
  mutate(NU_CPF = stringr::str_remove_all(NU_CPF, "[.-]")) 

sistec[1:500,] %>%  
  write.table("fake_data_sistec_1.csv", row.names = FALSE, sep = ";")

sistec[501:1017,] %>% 
  write.table("fake_data_sistec_2.csv", row.names = FALSE, sep = ";")  
