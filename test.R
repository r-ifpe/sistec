a <- sistec::read_sistec("C:/Pesquisa/sistec/inst/extdata/test_datasets/sistec/")
b <- sistec::read_qacademico("C:/Pesquisa/sistec/inst/extdata/test_datasets/qacademico/")


sistec_path <- system.file("extdata/test_datasets/sistec", package = "sistec")
qacademico_path <- system.file("extdata/test_datasets/qacademico", package = "sistec")

comparison <- compare_sistec(sistec_path, qacademico_path)




a <- sistec::compare_sistec("C:/Pesquisa/sistec/inst/extdata/test_datasets/sistec/",
                            "C:/Pesquisa/sistec/inst/extdata/test_datasets/qacademico/")



b <- sistec::read_qacademico("C:/Pesquisa/dados/qacademico/")
a <- sistec::read_sistec("C:/Pesquisa/dados/fake/sistec/", type = "complete")

b <- sistec::read_qacademico("C:/Pesquisa/dados/qacademico/", type = "complete")
a <- sistec::read_sistec("C:/Pesquisa/dados/sistec/")
a <- sistec::read_sistec("C:/Pesquisa/dados/sistec/web/")


a <- sistec::compare_sistec("C:/Pesquisa/sistec/inst/extdata/test_datasets/sistec/",
                            "C:/Pesquisa/sistec/inst/extdata/test_datasets/qacademico/",
                            output_path = "C:/Users/dmmad/Desktop/",
                            output_folder_name = "Sistec1")



a <- sistec::compare_sistec_qacademico_complete("C:/Pesquisa/dados/sistec/",
                                                "C:/Pesquisa/dados/qacademico/",
                                                write_output_path = "C:/Users/dmmad/Desktop/", 
                                                  )

aa <- sistec::compare_sistec_qacademico_complete("C:/Pesquisa/dados/fake/sistec/",
                                                "C:/Pesquisa/dados/fake/qacademico/",
                                                write_output_path = "C:/Pesquisa/dados/fake/",
                                                institute = "IFPE"
)

bb <- sistec::compare_sistec_qacademico_complete("C:/Pesquisa/dados/fake/sistec1/",
                                                "C:/Pesquisa/dados/fake/qacademico1/",
                                                write_output_path = "C:/Pesquisa/dados/fake/",
                                                institute = "IFPE1"
)

sistec <- utils::read.csv("C:/Pesquisa/dados/sistec/setec/Extração_dados_SISTEC_com CPF.csv", sep = ";",  stringsAsFactors = FALSE, 
                          encoding = "UTF-8", check.names = FALSE)

sistec <- utils::read.csv("C:/Pesquisa/dados/sistec/2020-03-05 IFPE.csv", sep = ";",  stringsAsFactors = FALSE, 
                          encoding = "UTF-8", check.names = FALSE)

sistec1 %>% distinct() %>%
  mutate(`Unidade Ensino` = stringr::str_sub(`Unidade Ensino`, 42)) %>% 
  mutate(`Unidade Ensino` = stringr::str_replace(`Unidade Ensino`, "REU ", "ABREU ")) %>% 
  write.csv("Co_unidade_ensino.csv", row.names = FALSE)

# Browse[1]> nrow(sistec)
# [1] 95067
# Browse[1]> nrow(qacademico)
# [1] 85447



qacademico <- sistec::read_qacademico("extdata/qacademico/")
sistec <- sistec::read_sistec("extdata/sistec/")

nrow(qacademico)

a <- compare_q_sistec(sistec_path = system.file("extdata/sistec", package = "sistec"),
                 qacademico_path = system.file("extdata/qacademico", package = "sistec"))



a <- read.csv("C:/Pesquisa/dados/sistec/2020-03-05 IFPE.csv", sep = ";",
              stringsAsFactors = FALSE, encoding = "UTF-8")
b1 <- read.csv("C:/Pesquisa/dados/qacademico/ListagemdeAlunos_2020_1_1.csv", sep = "",
              stringsAsFactors = FALSE, encoding = "latin1")

a <- sistec::read_sistec("C:/Pesquisa/dados/sistec/", type = "complete")
b <- sistec::read_qacademico("C:/Pesquisa/dados/qacademico/", type = "complete")

# no visible binding for global variable
b <- sistec::read_qacademico("C:/Pesquisa/dados/fake/qacademico/", type = "complete")
a <- sistec::read_sistec("C:/Pesquisa/dados/fake/sistec/", type = "complete")


b <- read.csv("C:/pesquisa/dados/qacademico/ListagemdeAlunos_2020_1_1.csv", header = TRUE,
              sep = "", stringsAsFactors = FALSE)
a <- read.csv("C:/pesquisa/dados/qacademico/ListagemdeAlunos_2019_2_1.csv", header = TRUE, sep = "")

b1 <- b %>% 
  select(Matrícula, Nome, Situação.Período, Situação.Matrícula,
         Curso, Cpf, Instituição, Per..Letivo.Inicial)

a1 <- a %>% 
  select(Matrícula, Nome, Situação.Período, Situação.Matrícula,
         Curso, Cpf, Instituição, Per..Letivo.Inicial)


q1 <- openxlsx::read.xlsx("arquivos/fake_data_qacademico_1.xlsx")
q2 <- openxlsx::read.xlsx("arquivos/fake_data_qacademico_2.xlsx") 

a <- read.csv("C:/Pesquisa/dados/sistec/2020-03-05 IFPE.csv",
              sep = ";",  stringsAsFactors = FALSE,  encoding = "UTF-8",
              colClasses = c("Numero.Cpf" = "character")) %>% 
  mutate(Numero.Cpf = ifelse(str_length(Numero.Cpf) == 0,
                                  Numero.Cpf,
                                  str_pad(Numero.Cpf, 11, pad = "0")))


complete_cpf <- function(cpf){
  browser()
  cpf_length <- stringr::str_length(cpf) 
  if(cpf_length == 11|cpf_length == 0){
    cpf
  } else {
    zeros <- 11 - cpf_length # a cpf always have 11 numbers
    zeros <- paste0(zeros, collapse = "")
    paste0(zeros, cpf)
  }
}


sistec %>% 
  select(NO_CICLO_MATRICULA) %>% 
  mutate(NO_CICLO_MATRICULA1 = stringr::str_remove(NO_CICLO_MATRICULA, " - .*$")) 

