compare_sistec_qacademico <- function(sistec, qacademico){
  sistec_complete <- sistec
  qacademico_complete <- qacademico

  # remove invalid cpf
  x <- filter_cpf_sistec(sistec)
  sistec <- x$sistec
  sistec_without_cpf <- x$sistec_without_cpf
  
  x <- filter_cpf_qacademico(qacademico)
  qacademico <- x$qacademico
  qacademico_without_cpf <- x$qacademico_without_cpf
  
  # remove unlinked cpf
  x <- unlinked_cpf_sistec_qacademico(sistec, qacademico)
  sistec <- x$sistec
  sistec_without_qacademico <- x$sistec_without_qacademico
  qacademico <- x$qacademico
  qacademico_without_sistec <- x$qacademico_without_sistec
  
  # join qacademico and sistec
  situation_to_update <- join_sistec_qacademico(sistec, qacademico)

  # linked courses between qacademico and sistec
  situation_to_update <- linked_courses_sistec_qacademico(situation_to_update)

  # remove cases unlinked course
  x <- unlinked_course_sistec_qacademico(sistec, qacademico, situation_to_update)
  sistec <- x$sistec
  sistec_without_qacademico <- dplyr::bind_rows(sistec_without_qacademico,
                                                x$sistec_without_link)
  qacademico <- x$qacademico
  qacademico_without_sistec <- dplyr::bind_rows(qacademico_without_sistec,
                                                x$qacademico_without_link)

  # compare student situation
  situation_to_update$Status <- compare_situation_sistec_qacademico(situation_to_update$Status_sistec,
                                                                    situation_to_update$Status_q)
  
  list(sistec_complete = sistec_complete,
       sistec_without_cpf = sistec_without_cpf,
       sistec_without_qacademico = sistec_without_qacademico,
       qacademico_complete = qacademico_complete,
       qacademico_without_cpf = qacademico_without_cpf,
       qacademico_without_sistec = qacademico_without_sistec,
       situation_to_update = situation_to_update)
}
