join %>% 
  group(Função, subfunção, acao, programa, ano) %>% 
  summariza(Pago - `Pago inicial`)

ifpe <- read_rfept("inst/extdata/test_datasets/generic_rfept/rfept/")


ifpe <- read_rfept("inst/extdata/test_datasets/qacademico/") %>% 
  filter(R_NO_CURSO == "ENGENHARIA MECÂNICA", R_DT_INICIO_CURSO == "2019.1")
sistec <- read_sistec("inst/extdata/test_datasets/sistec/") %>% 
  filter(S_NO_CURSO == "ENGENHARIA MECÂNICA", S_NO_CAMPUS == "RECIFE")
d <- compare_sistec(sistec, ifpe)

ifpe$R_CO_MATRICULA <- floor(runif(45, 0.1, 0.99) * 1000000)

ifpe %>% 
  select(-R_CO_CICLO_MATRICULA) %>% 
  write.table("ifpe.csv", sep = ",", row.names = FALSE, fileEncoding = "latin1")

ifpe2 <- read_rfept("oioi")
sistec2 <- read_sistec("oioi2")

d <- compare_sistec(sistec2, ifpe2)

write.table(sistec, "sistec.csv", sep = ",", row.names = FALSE, fileEncoding = "latin1")





library(sistec)

sigaa <- read_rfept("C:/Pesquisa/app_test/academico/") 
sistec <- read_sistec("C:/Pesquisa/app_test/sistec/")
d <- compare_sistec(sistec, sigaa)
write_output(d, "C:/Pesquisa/app_test/")




a <- read_sistec("C:/Users/dmmad/Desktop/teste_legal/sistec/")
b <- read_sigaa("C:/Users/dmmad/Desktop/teste_legal/sigaa")
d <- compare_sistec(a,b)
write_output(d, "C:/Users/dmmad/Desktop/Nova pasta/")

read_sistec("C:/Pesquisa/sistec2/inst/extdata/test_datasets/sistec/") 
read_sistec("C:/Pesquisa/dados/sistec/web/")
read_sistec("C:/Pesquisa/dados/sistec")
read_sistec("C:/Pesquisa/dados/ifsc/sistec/")
read_sistec("C:/Users/dmmad/Desktop/sigaa/sistec")

a <- read_sistec("C:/Users/dmmad/Desktop/aria_testes/viviane/sistec/")
b <- read_sigaa("C:/Users/dmmad/Desktop/aria_testes/viviane/sigaa")
d <- compare_sistec(a,b)


a <- read_sistec("C:/Users/dmmad/Desktop/exemplos/sistec/")
b <- read_qacademico("C:/Users/dmmad/Desktop/exemplos/qacademico/")
d <- compare_sistec(a,b)
write_output(d, "C:/Users/dmmad/Desktop/Nova pasta/")

a <- read_sistec("C:/Users/dmmad/Desktop/teste_sc/sistec")
b <- read_sigaa("C:/Users/dmmad/Desktop/teste_sc/sigaa")
d <- compare_sistec(a,b)
write_output(d, "C:/Users/dmmad/Desktop/Tenso_demais")

a <- read_sistec("C:/Users/dmmad/Desktop/oi/sistec")
b <- read_sigaa("C:/Users/dmmad/Desktop/oi/sigaa")
d <- compare_sistec(a,b)
write_output(d, "C:/Users/dmmad/Desktop/Tenso_demais2")

a <- read_sistec("C:/Users/dmmad/Desktop/oi/sistec")
b <- read_sigaa("C:/Users/dmmad/Desktop/oi/sigaa")
d <- compare_sistec(a,b)
write_output(d, "C:/Users/dmmad/Desktop/Tenso_demais2")

a <- read_sistec("inst/extdata/examples/sistec/")
b <- read_qacademico("inst/extdata/examples/qacademico/")
d <- compare_sistec(a,b)

a <- read_sistec("inst/extdata/test_datasets/sistec/")
b <- read_qacademico("inst/extdata/test_datasets/qacademico/")
d <- compare_sistec(a,b)


aria_desktop_build("C:/Users/dmmad/Desktop/ARIA_desktop", 
                   "C:/Users/dmmad/Desktop")


