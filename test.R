read_sistec("C:/Pesquisa/sistec2/inst/extdata/test_datasets/sistec/") 
read_sistec("C:/Pesquisa/dados/sistec/web/")
read_sistec("C:/Pesquisa/dados/sistec")
read_sistec("C:/Pesquisa/dados/ifsc/sistec/")
read_sistec("C:/Users/dmmad/Desktop/sigaa/sistec")


a <- read.csv("C:/Users/dmmad/Desktop/exemplos/qacademico/ListagemdeAlunos_2019_2_1.csv",
 check.names = FALSE, header = TRUE, sep = "")




a <- read_sistec("C:/Pesquisa/sistec2/inst/extdata/test_datasets/sistec/") 
b <- read_qacademico("C:/Pesquisa/sistec2/inst/extdata/test_datasets/qacademico/") 
e <- read_linked_courses("C:/Pesquisa/sistec2/inst/extdata/test_datasets/linked_courses/")

d1 <- compare_sistec(a, b, e)
d2 <- compare_sistec(a, b)

write_output(d1, "C:/Users/dmmad/Desktop", "TEST_1")
write_output(d2, "C:/Users/dmmad/Desktop", "TEST_2")

x$rfept 
x$sistec
x$linked_courses %>%
  select(-CURSO_SISTEC) %>% 
  inner_join(x$rfept, ., by =c("R_DT_INICIO_CURSO" = "INICIO",
                               "R_NO_CURSO" = "CURSO_RFEPT",
                               "R_NO_CAMPUS" = "CAMPUS")) %>% 
  inner_join(x$sistec, ., by = c("S_CO_CICLO_MATRICULA" = "CICLO",
                                 "S_NU_CPF" = "R_NU_CPF"))

################################
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


