
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

####################################################
############################### complete  ##########
####################################################

# sistec
a <- read.csv("C:/Pesquisa/dados/sistec/2020-03-05 IFPE.csv", sep = ";",
              stringsAsFactors = FALSE, encoding = "UTF-8",
              colClasses = c("Numero.Cpf" = "character")) %>% 
dplyr::transmute(`Nome Aluno` = Nome.Aluno, 
                `Numero Cpf` = Numero.Cpf,
                `Co Ciclo Matricula` = Co.Ciclo.Matricula, 
                `Situação Matricula` = Situação.Matricula, # Situação.Matricula
                `No Curso`= No.Curso, 
                `Dt Data Inicio` = Dt.Data.Inicio,
                `Unidade Ensino` = Unidade.Ensino)



a %>% 
  filter(stringr::str_detect(`Dt Data Inicio`, "2020|2019")) %>%
  write.table("fake_data_sistec_complete.csv", sep = ";", row.names = FALSE, fileEncoding = "UTF-8")

# qacademico
path = c("C:/Pesquisa/dados/qacademico/ListagemdeAlunos_2020_1_1.csv",
         "C:/Pesquisa/dados/qacademico/ListagemdeAlunos_2019_1_1.csv",
         "C:/Pesquisa/dados/qacademico/ListagemdeAlunos_2019_2_1.csv")

classes <- c(Cpf = "character")

for(i in 1:3){

  e <- utils::read.csv(path[i], sep = "",  stringsAsFactors = FALSE, 
                       encoding = "latin1", colClasses = classes, check.names = FALSE)
  
  e %>% 
    select(!!!syms(vars)) %>% 
    filter(stringr::str_detect(`Per. Letivo Inicial`, "2020|2019")) %>% 
    write.table(paste0("fake_data_qacademico_complete_",i, ".csv"),
                row.names = FALSE, fileEncoding = "latin1", sep = "")
  
}

### fake data
a <- sistec::read_qacademico("C:/Pesquisa/dados/fake/qacademico/", type = "complete")
b <- sistec::read_sistec("C:/Pesquisa/dados/fake/sistec/", type = "complete") %>% 
  mutate(NO_CAMPUS = paste0("INSTITUTO FEDERAL DE PERNAMBUCO - CAMPUS ", NO_CAMPUS))

cpfs_rnd <- NULL
nome_rnd <- NULL
cpfs <- c(a$Cpf, b$NU_CPF) %>% unique()
invalid <- c("", "   .   .   -  ", "___.___.___-__")
cpfs <- cpfs[!cpfs %in% invalid]

library(stringi)
library(rlang)
for (i in 1:length(cpfs)){
  cpf_rnd <- stringi::stri_rand_strings(1,11, pattern = "[0-9]") %>% 
    stringr::str_replace(pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
                         replacement = "\\1.\\2.\\3-")
  
  cpfs_rnd[i] <- cpf_rnd
  
  
  str <- stri_rand_strings(3, 6, "[a-zA-Z]")
  nome_rnd[i] <- paste(str[1], str[2], str[3])
}

cpfs_table <- data.frame(cpfs, cpfs_rnd, nome_rnd, stringsAsFactors = FALSE)

## qacademico
path = c("C:/Pesquisa/dados/fake/qacademico/fake_data_qacademico_complete_1.csv",
         "C:/Pesquisa/dados/fake/qacademico/fake_data_qacademico_complete_2.csv",
         "C:/Pesquisa/dados/fake/qacademico/fake_data_qacademico_complete_3.csv")

classes <- c(Cpf = "character")

qacademico1_nome <- a$Nome %>% unique()
qacademico1_nome_rnd <- NULL
for(j in 1:length(qacademico1_nome)){
  qacademico1_nome_rnd[j] <- paste(stri_rand_strings(3, 6, "[a-zA-Z]"), collapse = " ")
}

qacademico1_nome_tbl <- data.frame(Nome = qacademico1_nome, Nome_rnd = qacademico1_nome_rnd,
                                   stringsAsFactors = FALSE)

for(i in 1:3){
  # com cpf
  qacademico <- utils::read.csv(path[i], sep = "",  stringsAsFactors = FALSE, 
                                encoding = "latin1", colClasses = classes, check.names = FALSE) %>% 
    filter(!(!!sym("Cpf") %in% invalid)) %>% 
    left_join(cpfs_table, by = c("Cpf" = "cpfs")) %>% 
    mutate(Nome = nome_rnd, Cpf = cpfs_rnd) %>% 
    select(-cpfs_rnd, -nome_rnd, -`Situação Período`)
  
  # sem cpf
  qacademico1 <- utils::read.csv(path[i], sep = "",  stringsAsFactors = FALSE, 
                                encoding = "latin1", colClasses = classes, check.names = FALSE) %>% 
    filter(!!sym("Cpf") %in% invalid) %>%  
    left_join(qacademico1_nome_tbl, by = "Nome") %>%  
    mutate(Nome = Nome_rnd) %>% 
    select(-`Situação Período`, -Nome_rnd)
    
    
    bind_rows(qacademico, qacademico1) %>% 
      write.table(paste0("fake_data_qacademico_complete_",i, ".csv"),
                  row.names = FALSE, fileEncoding = "latin1", sep = "")
}

## sistec 

# com cpf
sistec <- b %>% 
  inner_join(cpfs_table, by = c("NU_CPF" = "cpfs")) %>% 
  filter(!(!!sym("NU_CPF") %in% invalid)) %>% 
  mutate(NO_ALUNO = nome_rnd, NU_CPF = cpfs_rnd, 
         CO_CICLO_MATRICULA, NO_STATUS_MATRICULA) %>% 
  mutate(NU_CPF = stringr::str_remove_all(NU_CPF, "[.-]")) %>% 
  select(-cpfs_rnd, -nome_rnd)

# sem cpf
sistec1 <- b %>% 
  filter(!!sym("NU_CPF") %in% invalid)

sistec1_nome <- sistec1$NO_ALUNO %>% unique()
sistec1_nome_rnd <- NULL
for(j in 1:length(sistec1_nome)){
  sistec1_nome_rnd[j] <- paste(stri_rand_strings(3, 6, "[a-zA-Z]"), collapse = " ")
}

sistec1_nome_tbl <- data.frame(NO_ALUNO = sistec1_nome, Nome_rnd = sistec1_nome_rnd,
                               stringsAsFactors = FALSE)

sistec1 <- sistec1 %>% 
  left_join(sistec1_nome_tbl, by = "NO_ALUNO") %>%  
  mutate(NO_ALUNO = Nome_rnd) %>% 
  select(-Nome_rnd)

sistec2 <- bind_rows(sistec, sistec1)

names(sistec2) <- c("Nome Aluno","Numero Cpf","Co Ciclo Matricula",
                   "Situação Matricula","No Curso","Dt Data Inicio","Unidade Ensino")

sistec2 %>%  
  write.table("fake_data_sistec_1.csv",  sep = ";", row.names = FALSE, fileEncoding = "UTF-8")

