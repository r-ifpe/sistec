

read_rfept("inst/extdata/test_datasets/generic_rfept/wrong_cota/")









library(sistec)

ifc <- read_rfept("C:/Users/dmmad/Desktop/ARIA-testes/ifc/luzerna/sigaa/") 

sistec <- read_sistec("C:/Users/dmmad/Desktop/ARIA-testes/ifc/luzerna/sistec/")

d <- compare_sistec(sistec, ifc)

write_output(d, "C:/Users/dmmad/Desktop/ARIA2/")







a <- read_rfept("C:/Users/dmmad/Desktop/ARIA-testes/ifmg/conecta/")
a1 <- a %>%
  filter(R_NO_CAMPUS == "Congonhas",
         R_DT_INICIO_CURSO %in% c("2019.1", "2019.2", "2020.1", "2020.2"))

a2 <- a1 %>% 
  arrange(R_NO_CURSO, R_DT_INICIO_CURSO)

b <- read_sistec("C:/Users/dmmad/Desktop/ARIA-testes/ifmg/sistec/")
b1 <- b %>% 
  filter(S_DT_INICIO_CURSO %in% c("2019.1", "2019.2", "2020.1", "2020.2"))

b2 <- b1 %>% 
  arrange(S_NO_CURSO, S_DT_INICIO_CURSO)

a2$R_NO_ALUNO <- b2$S_NO_ALUNO[1:751]
a2$R_NU_CPF <- b2$S_NU_CPF[1:751]

d <- compare_sistec(b2, a2)

write_output(d, "C:/Users/dmmad/Desktop/ARIA_IFMG")


names(a2)


##################
#### retenção ####
##################
a <- read.csv("C:/Users/dmmad/Desktop/ARIA-testes/sistec_csv_COM_cpf.csv", sep = ";")

a %>%
  select(NO_CICLO_MATRICULA, DT_DATA_INICIO, DT_DATA_FIM_PREVISTO, NO_STATUS_MATRICULA) %>% 
  mutate(NO_CURSO = sistec:::sistec_course_name(NO_CICLO_MATRICULA),
         DT_RETENCAO_ANO = 1 + as.numeric(substr(DT_DATA_FIM_PREVISTO, 1,4)),
         DT_RETENCAO_MES = as.numeric(substr(DT_DATA_FIM_PREVISTO, 6,7)),
         DT_ATUAL_ANO = as.numeric(substr(Sys.Date(), 1,4)),
         DT_ATUAL_MES = as.numeric(substr(Sys.Date(), 6,7))) %>% 
  mutate(TEMPO_EM_CURSO = 12 * (DT_ATUAL_ANO - DT_RETENCAO_ANO) + DT_ATUAL_MES - DT_RETENCAO_MES) %>% 
  filter(TEMPO_EM_CURSO >= 12, NO_STATUS_MATRICULA == "EM_CURSO") %>% 
  select(-DT_DATA_INICIO, -DT_DATA_FIM_PREVISTO, -NO_CICLO_MATRICULA) %>% 
  mutate(STATUS = "RETIDO") 


#(ano_atual - ano_base) * 12 + mes_atual - mes_base
#####################################################################




sigaa <- read_rfept("sigaa")
sistec <- read_sistec("sistec")
d <- compare_sistec(sistec, sigaa)
write_output(d, "ARIA")

sigaa <- read_rfept("sigaa_ifsc")
sistec <- read_sistec("sistec_ifsc")
d <- compare_sistec(sistec, sigaa)
write_output(d, "ARIA")

sigaa <- read_rfept("sigaa_ifsc2")
sistec <- read_sistec("sistec_ifsc2")
d <- compare_sistec(sistec, sigaa)
write_output(d, "ARIA")

sigaa <- read_rfept("sigaa_ifsc3")
sistec <- read_sistec("sistec_ifsc")
d <- compare_sistec(sistec, sigaa)
write_output(d, "ARIA")


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


