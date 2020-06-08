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


qacademico <- utils::read.csv("C:/Pesquisa/dados/qacademico/amostra/ListagemdeAlunos_2020_1_1.csv", sep = "",  stringsAsFactors = FALSE, 
                              encoding = "latin1", nrows = 1, check.names = FALSE)

sigaa <- utils::read.csv("C:/Pesquisa/dados/ifsc/sigaa/sigaa.csv", sep = ";",  stringsAsFactors = FALSE, 
                              encoding = "UTF-8",check.names = FALSE)

write.table(sigaa[, 1:5], file = "sigaa2.csv", sep = ";",row.names = FALSE, fileEncoding = "UTF-8")

