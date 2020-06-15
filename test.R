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

# sistec and rfept unlinked
sis <- dplyr::anti_join(x$sistec, x$sistec_rfept_linked,
                 by = c("S_NU_CPF", "S_NO_CURSO")) 

rfe <- dplyr::anti_join(x$rfept, x$sistec_rfept_linked, 
                  by = c("R_NU_CPF" = "S_NU_CPF", "R_NO_CURSO")) 

# não existem no outro sistema, juntar com x$sistec_without_rfept
sis2 <- anti_join(sis, rfe, by = c("S_NU_CPF" = "R_NU_CPF"))
rfe2 <- anti_join(rfe, sis, by = c("R_NU_CPF" = "S_NU_CPF"))

# new sis and rfe
sis3 <- semi_join(sis, rfe, by = c("S_NU_CPF" = "R_NU_CPF"))
rfe3 <- semi_join(rfe, sis, by = c("R_NU_CPF" = "S_NU_CPF"))

"000.377.192-41"


