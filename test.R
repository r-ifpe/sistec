a <- sistec::read_sistec("C:/Pesquisa/dados/ifsc/precila/sistec/")  

b <- sistec::read_sigaa("C:/Pesquisa/dados/ifsc/precila/sigaa/") 

d <- sistec::compare_sistec(a,b)
sistec::write_output(d, "C:/Users/dmmad/Desktop", "TEST_SC")


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


####################


b <- read_rfept("C:/Pesquisa/dados/qacademico/amostra/")

b <- read_sigaa("C:/Pesquisa/dados/ifsc/complete/sigaa/")
a <- read_sistec("C:/Pesquisa/dados/ifsc/complete/sistec/")

d <- sistec::compare_sistec(a,b)
sistec::write_output(d, "C:/Users/dmmad/Desktop", "TEST_2_SC")


