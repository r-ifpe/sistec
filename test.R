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

tesst <- function(){
  a <- read_sistec("C:/Pesquisa/dados/sistec/") %>% 
    dplyr::filter(S_DT_INICIO_CURSO >= "2019.1")
  b <- read_qacademico("C:/Pesquisa/dados/qacademico/amostra/") %>% 
    dplyr::filter(R_DT_INICIO_CURSO >= "2019.1")
  
  d <- compare_sistec(a,b)
  
  write_output(d, "C:/Users/dmmad/Desktop", "TEST_2")
}
#########################



t1 <- t %>% 
  group_nest(R_NO_CAMPUS, R_NO_CURSO)

rename_sistec_data_frame <- function(x){
  x %>% 
    dplyr::select(NOME = !!sym("S_NO_ALUNO"),
                  CPF = !!sym("S_NU_CPF"),
                  CICLO = !!sym("S_CO_CICLO_MATRICULA"),
                  CAMPUS = !!sym("S_NO_CAMPUS"),
                  CURSO = !!sym("S_NO_CURSO"))
}


t1 <- lapply(d[2:3], function(e){
  e %>% filter(S_NO_CAMPUS == "RECIFE", S_NO_CURSO == "MATEMÁTICA")
})

t2 <- lapply(d[5:6], function(e){
  e %>% filter(R_NO_CAMPUS == "RECIFE", R_NO_CURSO == "MATEMÁTICA")
})







a <- sistec::read_sistec("C:/Pesquisa/dados/ifsc/sistec/") %>% 
  dplyr::filter(S_DT_INICIO_CURSO >= "2019.1")
b <- sistec::read_qacademico("C:/Pesquisa/dados/qacademico/amostra/") %>% 
  dplyr::filter(R_DT_INICIO_CURSO >= "2019.1")


sigaa <- sistec::read_sigaa("C:/Pesquisa/dados/ifsc/sigaa/")
sigaa %>% 
  select(Matricula) %>% 
  mutate(test = stringr::str_count(Matricula)) %>% 
  filter(test == 10) %>% 
  mutate(test2 = stringr::str_sub(Matricula, 1, 5)) %>% 
  group_by(test2) %>% 
  tally() %>% as.data.frame()



sigaa %>% 
  select(Matricula) %>% 
  mutate(test = stringr::str_count(Matricula)) %>% 
  group_by(test) %>% tally()

if_else()


as.data.frame(date,  stringsAsFactors = FALSE) %>%
  mutate(mat_len = stringr::str_length(date)) %>%
  mutate(test = ifelse(mat_len == 12,
                       paste0(stringr::str_sub(date, 1, 4), ".", 
                              stringr::str_sub(date, 5,5)),
                       date))

mat_date_beginning <- function(mat){
  len = stringr::str_length(mat)
  
  dplyr::case_when(
    len == 12 ~ paste0(stringr::str_sub(mat, 1, 4), ".", 
                       stringr::str_sub(mat, 5, 5)),
    len == 10 ~ ifelse(stringr::str_sub(mat, 1, 3) == 200,
                      paste0(stringr::str_sub(mat, 1, 4), ".",
                             stringr::str_sub(mat, 5,5)),
                      paste0("20",
                             stringr::str_sub(mat, 1, 2), ".", 
                             stringr::str_sub(mat, 3,3))),
    len == 9 ~ ifelse(stringr::str_sub(mat, 2, 2) == 0,
                      paste0(stringr::str_sub(mat, 1, 4), ".",
                             stringr::str_sub(mat, 5,5)),
                      paste0("200",
                             stringr::str_sub(mat, 1, 1), ".",
                             stringr::str_sub(mat, 2, 2))),
    len == 8 ~ paste0("19",
                      stringr::str_sub(mat, 1, 2), ".", 
                      stringr::str_sub(mat, 3,3))
  )
}

4800 + 3514 + 1
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



as_tibble(date) %>%
  mutate(test= mat_date_beginning(value)) %>%
  filter(test == "2020.0") %>% 
  mutate(test2 = str_sub(value, 1, 3)) %>% 
  filter(test2 != "200")

as_tibble(date) %>%
  mutate(test= mat_date_beginning(value)) %>%
  mutate(len = str_length(value)) %>% 
  filter(len == 10) %>% 
  mutate(test2 = str_sub(value, 1, 3)) %>% 
  filter(test2 == "200")






