#' Make the pnp critics from sistec files
#'
#' This function gives the pnp critics that can be retrive using sistec information.
#' 
#' @param students A sistec students data frame from `read_sistec_student()`.
#' @param ciclo A sistec ciclo data frame from `read_ciclo()`.
#' 
#' @return A list with the critics
#' 
#' @importFrom dplyr %>%
#' @export
pnp_critics <- function(students, ciclo){
  create_pnp_critics_list(students, ciclo) %>%
    pnp_beginning_date_gt_beginning_ciclo_date()
   # pnp_duplicated_registry()
    # pnp_students_in_course_yet() %>%
    # pnp_wrong_course_name() %>%
    # pnp_zero_dropout() %>%
    # pnp_insufficient_time() %>%
    # pnp_wrong_ciclo_duration() %>%
    # pnp_newcomers_gt_enrolled() %>%
    # pnp_newcomers_gt_places()

}

#' @importFrom dplyr %>% sym
pnp_beginning_date_gt_beginning_ciclo_date <- function(x){
  critic <- x$sistec %>%
    dplyr::filter(
      format(as.Date(!!sym("C_DT_CRIACAO")), "%Y-%m-%d") <
        format(as.Date(!!sym("C_DT_INICIO")), "%Y-%m-%d")
    ) %>%
    dplyr::transmute(
      !!sym("C_NO_CAMPUS"),
      !!sym("S_NO_CICLO_MATRICULA"),
      !!sym("S_NO_ALUNO"),
      !!sym("S_NU_CPF"),
      CRITICA = "Aluno com data de matr\u00edcula anterior a data do in\u00edcio do ciclo"
    )

  x$pnp_student_critics <- rbind(x$pnp_student_critics, critic)
  x
}

# # @importFrom dplyr %>% sym
# create_pnp_critics_list <- function(students, ciclo, start) {
# 
#   sistec <- dplyr::full_join(students, ciclo,
#     by = c("S_CO_CICLO_MATRICULA" = "C_CO_CICLO_MATRICULA"))
# 
#   if (!is.null(start)) {
#     sistec <- sistec %>% 
#       dplyr::mutate(
#         DT_YEAR_SEMESTER = sistec_convert_beginning_date(!!sym("C_DT_INICIO"))
#       ) %>% 
#       dplyr::filter(DT_YEAR_SEMESTER >= start) 
#     
#     sistec$DT_YEAR_SEMESTER <- NULL
#   }
#   
#   list(
#     sistec = sistec,
#     pnp_student_critics = data.frame(),
#     pnp_ciclo_critics = data.frame()
#   )
# }
# 
# ####################
# # in progess
# #####################
# 
# 
# # @importFrom dplyr %>%  sym
# pnp_wrong_course_name <- function(x){
#   wrong_names <- toupper(c(
#     "semin\u00e1rio", "encontro", "olimp\u00edada", "a\u00e7\u00e3o solid\u00e1ria",
#     "palestra", "cerim\u00f4nia", "semana", "tert\u00falia", "cavalgada", "jornada",
#     "experi\u00eancia", "f\u00f3rum", "f\u00f3runs", "circuito", "visita", "concurso",
#     "treinamento", "simp\u00f3sio", "ciclo", "apresenta\u00e7\u00e3o", "gincana",
#     "festival", "cinema comentado", "#CURTINDOOBMEP", "feira", "dia de campo", "IFshow",
#     "EMPREENTEC", "AMAZON AWS", "busca", "chamada", "exposi\u00e7\u00e3o", "voleibol","geog"
#   ))
#   
#   critic <- x$sistec %>%
#     dplyr::distinct(!!sym("NO_CICLO_MATRICULA"), MUNICIPIO) %>% 
#     dplyr::filter(grepl(paste0(wrong_names, collapse = "|"), NO_CICLO_MATRICULA)) %>% 
#     dplyr::mutate(Critica = paste0(
#       "Verifique o nome: ",
#       stringr::str_extract(NO_CICLO_MATRICULA, paste0(wrong_names, collapse = "|"))
#     ))
#   
#   x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
#   x
# }
# 
# # # @importFrom dplyr %>%
# # pnp_students_in_course_yet <- function(x) {
# #   critic <- x$sistec %>%
# #     dplyr::filter(NO_STATUS_MATRICULA == "EM_CURSO") %>%
# #     dplyr::mutate(DAYS = difftime(
# #       as.Date(Sys.time()),
# #       as.Date(!!sym("C_DT_FIM")),
# #       units = "days"
# #     )) %>%
# #     dplyr::mutate(CRITICA = dplyr::case_when(
# #       grepl("FORMAÇÃO CONTINUADA|FORMAÇÃO INICIAL", !!sym("C_NO_SUBTIPO")) & DAYS > 0 ~
# #         paste0("Aluno retido: ", DAYS, " dias."),
# #       !grepl("FORMAÇÃO CONTINUADA|FORMAÇÃO INICIAL", !!sym("C_NO_SUBTIPO")) & DAYS > 365 ~
# #         paste0("Aluno retido: ", DAYS, " dias."),
# #       TRUE ~ "OK"
# #     )) %>%
# #     dplyr::filter(CRITICA != "OK") %>%
# #     dplyr::select(C_NO_CAMPUS , S_NO_CICLO_MATRICULA, S_NO_ALUNO, S_NU_CPF, CRITICA)
# # 
# #   x$pnp_student_critics <- rbind(x$pnp_student_critics, critic)
# #   x
# # }
# # 

# # 
# # 
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# # ######################################
# # ## 3) EVASÃO ZERO
# # ######################################
# # 
# # pnp_zero_dropout <- function(x){
# #   critic <- x$sistec %>% 
# #     mutate(EVASION = dplyr::if_else(NO_STATUS_MATRICULA == "ABANDONO", 1, 0)) %>% 
# #     group_by(NO_CICLO_MATRICULA, MUNICIPIO) %>% 
# #     summarise(Total = n(), Evasion = sum(EVASION)) %>% 
# #     ungroup() %>% 
# #     filter(Evasion == 0) %>% 
# #     transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
# #               MUNICIPIO, 
# #               Critica = "Verifique a evasão zero")
# #   
# #   x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
# #   x
# # }
# # 
# # ######################################
# # ## 4) CARGA HORARIA INSUFICIENTE
# # ######################################
# # 
# # pnp_insufficient_time <- function(x){
# #   critic <- x$sistec %>% 
# #     filter(CARGA.HORÁRIA.TOTAL < 20 ) %>% 
# #     group_by(NO_CICLO_MATRICULA, MUNICIPIO) %>% 
# #     summarise(Critica = "Verificar carga horária") %>% 
# #     ungroup() %>% 
# #     transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
# #               MUNICIPIO, 
# #               Critica)
# #   
# #   x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
# #   x
# # }
# # 
# # ######################################
# # ## 6) Duração do Ciclo Imprópria
# # ######################################
# # 
# # pnp_wrong_ciclo_duration <- function(x){
# #   critic <- x$sistec %>% 
# #     mutate(DAYS = difftime(
# #       DATA.FIM.PREVISTO.DO.CURSO,
# #       DATA.INÍCIO.DO.CURSO,
# #       units = "days"
# #     )) %>% 
# #     distinct(NO_CICLO_MATRICULA, MUNICIPIO, SUBTIPO.CURSOS, DAYS) %>% 
# #     mutate(Critica = case_when(
# #       SUBTIPO.CURSOS == "FORMAÇÃO CONTINUADA" & (DAYS < 3 | DAYS > 365) ~ 
# #         paste0("Formação continuada: duração do ciclo entre 3 dias e 1 ano. Ciclo com ", DAYS, " dias."),
# #       SUBTIPO.CURSOS == "FORMAÇÃO INICIAL" & (DAYS < 3 | DAYS > 365) ~
# #         paste0("Formação inicial: duração do ciclo entre 3 dias e 1 ano. Ciclo com ", DAYS, " dias."),
# #       SUBTIPO.CURSOS == "TÉCNICO" & (DAYS < 365 | DAYS > 1460) ~ 
# #         paste0("Curso técnico: duração do ciclo entre 1 e 4 anos. Ciclo com ", DAYS, " dias."),
# #       SUBTIPO.CURSOS == "TECNOLOGIA" & (DAYS < 730 | DAYS > 1460) ~
# #         paste0("Tecnólogo: duração do ciclo entre 2 e 4 anos. Ciclo com ", DAYS, " dias."),
# #       SUBTIPO.CURSOS == "BACHARELADO" & (DAYS < 1460 | DAYS > 1825) ~ 
# #         paste0("Bacharelado: duração do ciclo entre 4 e 5 anos. Ciclo com ", DAYS, " dias."),
# #       SUBTIPO.CURSOS == "ESPECIALIZAÇÃO (LATO SENSU)" & (DAYS < 182.5 | DAYS > 730) ~ 
# #         paste0("Especialização: duração do ciclo entre 6 meses e 2 anos. Ciclo com ", DAYS, " dias."),
# #       SUBTIPO.CURSOS == "MESTRADO PROFISSIONAL" & (DAYS < 365 | DAYS > 730) ~ 
# #         paste0("Mestrado profissional: duração do ciclo entre 1 e 2 anos. Ciclo com ", DAYS, " dias."),
# #       SUBTIPO.CURSOS == "DOUTORADO" & (DAYS < 1095 | DAYS > 1460) ~ 
# #         paste0("Doutorado: duração do ciclo entre 3 e 4 anos. Ciclo com ", DAYS, " dias."),
# #       TRUE ~ "OK"
# #     )) %>% 
# #     filter(Critica != "OK") %>% 
# #     transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
# #               MUNICIPIO, 
# #               Critica) 
# #   
# #   x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
# #   x
# # }
# # 
# # ######################################
# # ## 7) Ingressantes > vagas
# # ###################################### 
# # 
# # pnp_newcomers_gt_places <- function(x){
# #   critic <- x$sistec %>% 
# #     filter(QTD.DE.MATRICULAS > QTD.DE.VAGAS) %>% 
# #     transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
# #               MUNICIPIO, 
# #               Critica = "A quantidade de ingressantes é maior que a quantidade de vagas") %>% 
# #     distinct()
# #   
# #   x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
# #   x
# # }
# # 
# # 
# # ######################################
# # ## 8) Ingressantes > inscritos
# # ###################################### 
# # 
# # pnp_newcomers_gt_enrolled <- function(x){
# #   critic <- x$sistec %>% 
# #     filter(QTD.DE.MATRICULAS > QTD.DE.INSCRITOS) %>% 
# #     transmute(NOME.DO.CURSO = NO_CICLO_MATRICULA, 
# #               MUNICIPIO, 
# #               Critica = "A quantidade de ingressantes é maior que a quantidade de inscritos") %>% 
# #     distinct()
# #   
# #   x$pnp_ciclo_critics <- rbind(x$pnp_ciclo_critics, critic)
# #   x
# # }
# # 
# # ##############################################
# # ## 11) Turnos de Oferta do Ciclo (igual a 10)
# # ############################################## 
# # 
# # ##############################################
# # ## 12) Aluno Duplicado 
# # ############################################## 
# # 
# # pnp_duplicated_registry <- function(x){
# #   critic <- x$sistec %>% 
# #     group_by(NO_CICLO_MATRICULA, MUNICIPIO, NO_ALUNO, NU_CPF) %>% 
# #     tally() %>% 
# #     ungroup() %>% 
# #     filter(n > 1) %>% 
# #     transmute(MUNICIPIO,
# #               NO_CICLO_MATRICULA,  
# #               NO_ALUNO, 
# #               NU_CPF,
# #               Critica = "Aluno com duas matrículas no mesmo ciclo.") 
# #   
# #   x$pnp_student_critics <- rbind(x$pnp_student_critics, critic)
# #   x
# # }
# # 
# # 
# # 
# # ############################################################
# # ## 15) Informar Cor/Raça (feita de maneira manual)
# # ############################################################
# # 
# # ############################################################
# # ## 16) Informar Renda per capita (feita de maneira manual)
# # ############################################################
# # 
# # ############################################################
# # ## 17) Informar Turno de Aluno (feita de maneira manual)
# # ############################################################
# # 
# # 
# # ############
# # ## running 
# # ############
# # 
# # ciclo <- read.csv("inst/extdata/test_datasets/pnp_critics/ciclo-matricula_fake.csv", sep = ";")
# # students <- read.csv("inst/extdata/test_datasets/pnp_critics/sistec_fake.csv", sep = ";")
# # 
# # 
# # 
# # ciclo <- read.csv("ciclo-matricula.csv", sep = ";")
# # students <- read.csv("sistec.csv", sep = ";")
# # 
# # 
# # sistec <- dplyr::full_join(ciclo, students, 
# #                            by = c("CÓDIGO.CICLO.DE.MATRÍCULA" = "CO_CICLO_MATRICULA"))
# # 
# # 
# # pnp <- list(
# #   sistec = sistec,
# #   pnp_student_critics = data.frame(),
# #   pnp_ciclo_critics = data.frame()
# # )
# # 
# # pnp_wrong_course_name(pnp)
# # 
# # pnp <- pnp %>% 
# #   pnp_beginning_date_gt_beginning_ciclo_date() %>% 
# #   pnp_duplicated_registry() %>% 
# #   pnp_students_in_course_yet() %>% 
# #   pnp_wrong_course_name() %>% 
# #   pnp_zero_dropout() %>% 
# #   pnp_insufficient_time() %>% 
# #   pnp_wrong_ciclo_duration() %>% 
# #   pnp_newcomers_gt_enrolled() %>% 
# #   pnp_newcomers_gt_places() 
# # 
# # pnp$pnp_student_critics %>%
# #   na.omit() %>% 
# #   mutate(NO_ALUNO = str_trim(NO_ALUNO)) %>% 
# #   arrange(MUNICIPIO, NO_CICLO_MATRICULA, NO_ALUNO, Critica)
# # 
# # pnp$pnp_ciclo_critics %>% 
# #   na.omit() %>% 
# #   select(MUNICIPIO, NOME.DO.CURSO, Critica) %>% 
# #   arrange(MUNICIPIO, NOME.DO.CURSO, Critica) %>% as_tibble()
# # 
# # 
