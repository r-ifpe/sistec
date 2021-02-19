########
## Criticas feitas
## 
## 2) Nome de Curso Impróprio
## 3) Evasão 0%
## 4) Carga Horária Insuficiente
## 6) Duração do Ciclo Imprópria
## 7) Detalhamento do número de vagas
## 8) Ingressantes > Inscritos
## 9) Turnos de Oferta do Ciclo (feita de maneira manual)
## 10) Data da Matrícula Incoerente
## 11) Matrícula posterior à "ocorrência da situação" (igual a critica 10)
## 12) Aluno Duplicado 

## 15) Informar Cor/Raça (feita de maneira manual)
## 16) Informar Renda per capita (feita de maneira manual)
## 17) Informar Turno de Aluno (feita de maneira manual)




########################################
## 2) Nome de Curso Impróprio
########################################

pnp_wrong_course_name <- function(x){
  wrong_names <- toupper(c(
    "semin\u00e1rio", "encontro", "olimp\u00edada", "a\u00e7\u00e3o solid\u00e1ria",
    "palestra", "cerim\u00f4nia", "semana", "tert\u00falia", "cavalgada", "jornada",
    "experi\u00eancia", "f\u00f3rum", "f\u00f3runs", "circuito", "visita", "concurso",
    "treinamento", "simp\u00f3sio", "ciclo", "apresenta\u00e7\u00e3o", "gincana",
    "festival", "cinema comentado", "#CURTINDOOBMEP", "feira", "dia de campo", "IFshow",
    "EMPREENTEC", "AMAZON AWS", "busca", "chamada", "exposi\u00e7\u00e3o", "voleibol","geog"
  ))
  
  critic <- x$sistec %>%
    select(NOME.DO.CURSO, MUNICIPIO) %>% 
    distinct() %>% 
    filter(grepl(paste0(wrong_names, collapse = "|"), NOME.DO.CURSO)) %>% 
    mutate(Critica = paste0(
      "Verifique o nome: ",
      str_extract(NOME.DO.CURSO, paste0(wrong_names, collapse = "|"))
    ))
  
  x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
  x
}


######################################
## 3) EVASÃO ZERO
######################################

pnp_zero_dropout <- function(x){
  critic <- x$sistec %>% 
    mutate(EVASION = dplyr::if_else(NO_STATUS_MATRICULA == "ABANDONO", 1, 0)) %>% 
    group_by(NO_CICLO_MATRICULA, MUNICIPIO) %>% 
    summarise(Total = n(), Evasion = sum(EVASION)) %>% 
    ungroup() %>% 
    filter(Evasion == 0) %>% 
    transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
           MUNICIPIO, 
           Critica = "Verifique a evasão zero")
  
  x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
  x
}

######################################
## 4) CARGA HORARIA INSUFICIENTE
######################################

pnp_insufficient_time <- function(x){
  critic <- x$sistec %>% 
    filter(CARGA.HORÁRIA.TOTAL < 20 ) %>% 
    group_by(NO_CICLO_MATRICULA, MUNICIPIO) %>% 
    summarise(Critica = "Verificar carga horária") %>% 
    ungroup() %>% 
    transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
              MUNICIPIO, 
              Critica)
  
  x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
  x
}

######################################
## 6) Duração do Ciclo Imprópria
######################################

pnp_wrong_ciclo_duration <- function(x){
  critic <- x$sistec %>% 
    mutate(DAYS = difftime(
      DATA.FIM.PREVISTO.DO.CURSO,
      DATA.INÍCIO.DO.CURSO,
      units = "days"
    )) %>% 
    distinct(NO_CICLO_MATRICULA, MUNICIPIO, SUBTIPO.CURSOS, DAYS) %>% 
    mutate(Critica = case_when(
      SUBTIPO.CURSOS == "FORMAÇÃO CONTINUADA" & (DAYS < 3 | DAYS > 365) ~ 
        paste0("Formação continuada: duração do ciclo entre 3 dias e 1 ano. Ciclo com ", DAYS, " dias."),
      SUBTIPO.CURSOS == "FORMAÇÃO INICIAL" & (DAYS < 3 | DAYS > 365) ~
        paste0("Formação inicial: duração do ciclo entre 3 dias e 1 ano. Ciclo com ", DAYS, " dias."),
      SUBTIPO.CURSOS == "TÉCNICO" & (DAYS < 365 | DAYS > 1460) ~ 
        paste0("Curso técnico: duração do ciclo entre 1 e 4 anos. Ciclo com ", DAYS, " dias."),
      SUBTIPO.CURSOS == "TECNOLOGIA" & (DAYS < 730 | DAYS > 1460) ~
        paste0("Tecnólogo: duração do ciclo entre 2 e 4 anos. Ciclo com ", DAYS, " dias."),
      SUBTIPO.CURSOS == "BACHARELADO" & (DAYS < 1460 | DAYS > 1825) ~ 
        paste0("Bacharelado: duração do ciclo entre 4 e 5 anos. Ciclo com ", DAYS, " dias."),
      SUBTIPO.CURSOS == "ESPECIALIZAÇÃO (LATO SENSU)" & (DAYS < 182.5 | DAYS > 730) ~ 
        paste0("Especialização: duração do ciclo entre 6 meses e 2 anos. Ciclo com ", DAYS, " dias."),
      SUBTIPO.CURSOS == "MESTRADO PROFISSIONAL" & (DAYS < 365 | DAYS > 730) ~ 
        paste0("Mestrado profissional: duração do ciclo entre 1 e 2 anos. Ciclo com ", DAYS, " dias."),
      SUBTIPO.CURSOS == "DOUTORADO" & (DAYS < 1095 | DAYS > 1460) ~ 
        paste0("Doutorado: duração do ciclo entre 3 e 4 anos. Ciclo com ", DAYS, " dias."),
      TRUE ~ "OK"
    )) %>% 
    filter(Critica != "OK") %>% 
    transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
              MUNICIPIO, 
              Critica) 
  
  x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
  x
}

######################################
## 7) Ingressantes > vagas
###################################### 

pnp_newcomers_gt_places <- function(x){
  critic <- x$sistec %>% 
    filter(QTD.DE.MATRICULAS > QTD.DE.VAGAS) %>% 
    transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
              MUNICIPIO, 
              Critica = "A quantidade de ingressantes é maior que a quantidade de vagas") %>% 
    distinct()
  
  x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
  x
}


######################################
## 8) Ingressantes > inscritos
###################################### 

pnp_newcomers_gt_enrolled <- function(x){
  critic <- x$sistec %>% 
    filter(QTD.DE.MATRICULAS > QTD.DE.INSCRITOS) %>% 
    transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
              MUNICIPIO, 
              Critica = "A quantidade de ingressantes é maior que a quantidade de inscritos") %>% 
    distinct()
  
  x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
  x
}

#########################################
## 9) Turnos de Oferta do Ciclo (manual)
######################################### 


###################################### 
## 10) Data da Matrícula Incoerente
###################################### 

pnp_beginning_date_gt_beginning_ciclo_date <- function(x){
  critic <- x$sistec %>% 
    filter(
      format(as.Date(DATA_CRIACAO), "%Y-%m") <
      format(as.Date(DT_DATA_INICIO), "%Y-%m")
    ) %>% 
    transmute(MUNICIPIO, 
              NO_CICLO_MATRICULA, 
              NO_ALUNO, 
              NU_CPF, 
              Critica = "Aluno com data de matrícula anterior a data do início do ciclo"
    ) 
  
  x$pnp_student_critics <- rbind(x$pnp_student_critics, critic)
  x
}

##############################################
## 11) Turnos de Oferta do Ciclo (igual a 10)
############################################## 

##############################################
## 12) Aluno Duplicado 
############################################## 

pnp_duplicated_registry <- function(x){
  critic <- x$sistec %>% 
    group_by(NO_CICLO_MATRICULA, MUNICIPIO, NO_ALUNO, NU_CPF) %>% 
    tally() %>% 
    ungroup() %>% 
    filter(n > 1) %>% 
    transmute(MUNICIPIO,
              NO_CICLO_MATRICULA,  
              NO_ALUNO, 
              NU_CPF,
              Critica = "Aluno com duas matrículas no mesmo ciclo.") 
  
  x$pnp_student_critics <- rbind(x$pnp_student_critics, critic)
  x
}


##############################################
## 13 e 14) Retenção Crítica (regular e FIC)
############################################## 

pnp_students_in_course_yet <- function(x) {
  critic <- x$sistec %>% 
    filter(NO_STATUS_MATRICULA == "EM_CURSO") %>% 
    mutate(DAYS = difftime(
      as.Date(Sys.time()),
      as.Date(DT_DATA_FIM_PREVISTO),
      units = "days"  
    )) %>% 
    mutate(Critica = case_when(
      grepl("FORMAÇÃO CONTINUADA|FORMAÇÃO INICIAL", SUBTIPO.CURSOS) & DAYS > 0 ~
        paste0("Aluno retido: ", DAYS, " dias."), 
      !grepl("FORMAÇÃO CONTINUADA|FORMAÇÃO INICIAL", SUBTIPO.CURSOS) & DAYS > 365 ~
        paste0("Aluno retido: ", DAYS, " dias."), 
      TRUE ~ "OK"
    )) %>% 
    filter(Critica != "OK") %>% 
    select(MUNICIPIO, NO_CICLO_MATRICULA, NO_ALUNO, NU_CPF, Critica) 
  
  x$pnp_student_critics <- rbind(x$pnp_student_critics, critic)
  x
}

############################################################
## 15) Informar Cor/Raça (feita de maneira manual)
############################################################

############################################################
## 16) Informar Renda per capita (feita de maneira manual)
############################################################

############################################################
## 17) Informar Turno de Aluno (feita de maneira manual)
############################################################


############
## running 
############

ciclos <- read.csv("sistec/ciclo-matricula.csv", sep =  ";")
students <- read.csv("sistec/sistec.csv", sep =  ";")
sistec <- full_join(students, ciclos, 
                 by = c("CO_CICLO_MATRICULA" = "CÓDIGO.CICLO.DE.MATRÍCULA")
)


pnp <- list(
  sistec = sistec,
  pnp_student_critics = data.frame(),
  pnp_ciclo_critics = data.frame()
)



pnp <- pnp %>% 
  pnp_beginning_date_gt_beginning_ciclo_date() %>% 
  pnp_duplicated_registry() %>% 
  pnp_students_in_course_yet() %>% 
  pnp_wrong_course_name() %>% 
  pnp_zero_dropout() %>% 
  pnp_insufficient_time() %>% 
  pnp_wrong_ciclo_duration() %>% 
  pnp_newcomers_gt_enrolled() %>% 
  pnp_newcomers_gt_places() 
  
pnp$pnp_student_critics %>%
  na.omit() %>% 
  mutate(NO_ALUNO = str_trim(NO_ALUNO)) %>% 
  arrange(MUNICIPIO, NO_CICLO_MATRICULA, NO_ALUNO, Critica)

pnp$pnp_ciclo_critics %>% 
  na.omit() %>% 
  select(MUNICIPIO, NOME.DO.CURSO, Critica) %>% 
  arrange(MUNICIPIO, NOME.DO.CURSO, Critica) %>% as_tibble()


