qacademico <- sistec::read_qacademico("extdata/qacademico/")
sistec <- sistec::read_sistec("extdata/sistec/")

nrow(qacademico)

a <- compare_q_sistec(sistec_path = system.file("extdata/sistec", package = "sistec"),
                 qacademico_path = system.file("extdata/qacademico", package = "sistec"))












b <- read.csv("C:/pesquisa/dados/qacademico/ListagemdeAlunos_2020_1_1.csv", header = TRUE, sep = "")
a <- read.csv("C:/pesquisa/dados/qacademico/ListagemdeAlunos_2019_2_1.csv", header = TRUE, sep = "")

b1 <- b %>% 
  select(Matrícula, Nome, Situação.Período, Situação.Matrícula,
         Curso, Cpf, Instituição, Per..Letivo.Inicial)

a1 <- a %>% 
  select(Matrícula, Nome, Situação.Período, Situação.Matrícula,
         Curso, Cpf, Instituição, Per..Letivo.Inicial)


q1 <- openxlsx::read.xlsx("arquivos/fake_data_qacademico_1.xlsx")
q2 <- openxlsx::read.xlsx("arquivos/fake_data_qacademico_2.xlsx") 


t <- bind_rows(b1,a1)

Matrícula
Nome
Situação.Matrícula
Situação.Periodo
Curso
Cpf
Instituiãção
Per..Letivo.Inicial  

a1 <- a %>% 
  select(Matrícula, Nome, Situação.Matrícula, Situação.Período,  
         Per..Letivo.Inicial, Curso, Cpf, Instituição)


b1 <- b %>% 
  select(Matrícula, Nome, Situação.Matrícula, Situação.Período,  
         Per..Letivo.Inicial, Curso, Cpf, Instituição)

t <- bind_rows(a1,b1)

t %>% 
  tidyr::separate( Per..Letivo.Inicial, sep = "/", c("ano_inicial", "periodo_inicial"),
                   remove = FALSE) %>% 
  select(Per..Letivo.Inicial, ano_inicial, periodo_inicial)
 

sistec <- read.csv("C:/Pesquisa/dados/sistec/2020-03-05 IFPE.csv", header = TRUE,
                   encoding = "UTF-8")

# no sistec
`Co Ciclo Matricula`
`Dt Data Inicio` 
`No Curso`
`Nome Aluno` 
`Numero Cpf`
`Situação Matricula` 


# alunos sem cpf
# alunos com matricula diferente do formate 20201XXXXXXX
# renomear arquivos de modo que a maior data seja o nome do arquivo. Ex
#   Se a maior data for 2019 e semestre 1, sei que é um arquivo de 2019.1 
