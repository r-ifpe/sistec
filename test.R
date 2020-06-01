#a <- sistec::read_sistec("C:/Pesquisa/sistec/inst/extdata/test_datasets/sistec/")
a <- sistec::read_sistec("C:/Pesquisa/dados/sistec/web") %>% 
  dplyr::filter(S_DT_INICIO_CURSO >= "2019.1")
b <- sistec::read_qacademico("C:/Pesquisa/dados/qacademico/amostra/") %>% 
  dplyr::filter(R_DT_INICIO_CURSO >= "2019.1")
#b <- sistec::read_qacademico("C:/Pesquisa/sistec/inst/extdata/test_datasets/qacademico/")

d <- sistec::compare_sistec(a,b)
sistec::write_output(d, "C:/Users/dmmad/Desktop", "TEST")


################
b <- sistec::read_qacademico("C:/Pesquisa/dados/qacademico/amostra/")
d <- sistec:::extract_cpfs(b)

sistec::write_output(d, "C:/Users/dmmad/Desktop", "TEST")









