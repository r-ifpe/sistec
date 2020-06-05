a <- sistec::read_sistec("C:/Pesquisa/dados/ifsc/sistec/") %>% 
  filter(S_DT_INICIO_CURSO >= "2019.1")
b <- sistec::read_sigaa("C:/Pesquisa/dados/ifsc/sigaa/") %>% 
  filter(R_DT_INICIO_CURSO >= "2019.1")

d <- sistec::compare_sistec(a,b)
sistec::write_output(d, "C:/Users/dmmad/Desktop", "TEST_SC")


a <- read_sistec("C:/Pesquisa/dados/sistec/") %>% 
  dplyr::filter(S_DT_INICIO_CURSO >= "2019.1")
b <- read_qacademico("C:/Pesquisa/dados/qacademico/amostra/") %>% 
  dplyr::filter(R_DT_INICIO_CURSO >= "2019.1")

d <- compare_sistec(a,b)

write_output(d, "C:/Users/dmmad/Desktop", "TEST_2")

qacademico <- utils::read.csv("inst/extdata/test_datasets/qacademico/fake_data_qacademico_2019_2.csv", sep = "",  stringsAsFactors = FALSE, 
                encoding = "latin1", check.names = FALSE)

qacademico2 <- utils::read.csv("C:/Pesquisa/dados/qacademico/amostra/ListagemdeAlunos_2019_2_1.csv", sep = "",  stringsAsFactors = FALSE, 
                               encoding = "latin1", check.names = FALSE)

qacademico3 <- qacademico2 %>% filter(`Per. Letivo Inicial` >= "2019.1")

cota <- qacademico3 %>% select(Cota) 

qacademico5 <- qacademico %>% bind_cols(cota)

write.table(qacademico5, "fake_data_qacademico_2019_2.csv",
            row.names = FALSE, fileEncoding = "latin1", sep = "")


qacademico_example <- utils::read.csv("inst/extdata/examples/qacademico/fake_data_qacademico.csv", sep = "",  stringsAsFactors = FALSE, 
                                      encoding = "latin1", check.names = FALSE) 

qacademico_test2 <- utils::read.csv("inst/extdata/test_datasets/qacademico/fake_data_qacademico_2019_2.csv", sep = "",  stringsAsFactors = FALSE, 
                                      encoding = "latin1", check.names = FALSE) 

qacademico_test1 <- utils::read.csv("inst/extdata/test_datasets/qacademico/fake_data_qacademico_2019_1.csv", sep = "",  stringsAsFactors = FALSE, 
                                    encoding = "latin1", check.names = FALSE) 
write.table(t1, "fake_data_qacademico.csv",
            row.names = FALSE, fileEncoding = "latin1", sep = "")

############
a <- read_sistec("C:/Pesquisa/sistec2/inst/extdata/test_datasets/sistec/") %>% 
  dplyr::filter(S_DT_INICIO_CURSO >= "2019.1")
b <- read_qacademico("C:/Pesquisa/sistec2/ova pasta/") %>% 
  dplyr::filter(R_DT_INICIO_CURSO >= "2019.1")

d <- compare_sistec(a,b)



