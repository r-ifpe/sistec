a <- read_sistec("C:/Users/dmmad/Desktop/teste_legal/sistec/")
b <- read_sigaa("C:/Users/dmmad/Desktop/teste_legal/sigaa")
d <- compare_sistec(a,b)

t1 <- x$sistec_duplicated_registry %>% # ver o campus
  inner_join(x$linked_courses,
             by = c("S_NO_CURSO" = "S_NO_CURSO_LINKED",
                    "S_DT_INICIO_CURSO" = "R_DT_INICIO_CURSO",
                    "S_CO_CICLO_MATRICULA")) %>% 
  inner_join(x$rfept, 
             by = c("S_NU_CPF" = "R_NU_CPF",
                    "S_DT_INICIO_CURSO" = "R_DT_INICIO_CURSO",
                    "R_NO_CURSO")) %>% 
  transmute(R_NO_CURSO, S_NO_CURSO_LINKED = S_NO_CURSO,
            )





a <- read_sistec("C:/Users/dmmad/Desktop/Nova pasta/sistec/")
b <- read_qacademico("C:/Users/dmmad/Desktop/Nova pasta/qacademico/")
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


