#' @export
compare_sistec_qacademico_complete <- function(sistec_path, qacademico_path,
                                               type = "complete",
                                               write_output_path = "",
                                               institute = "IFPE"){
  
  # read files
  sistec <- read_sistec(path = sistec_path, type = type)
  qacademico <- read_qacademico(path = qacademico_path, type = type) %>% # I found this problem 
    dplyr::mutate(Campus = ifelse(Campus == "", "SEM CAMPUS", Campus))   # in qacademico

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

  # remove cases without linked course
  x <- unlinked_course_sistec_qacademico(sistec, qacademico, situation_to_update)
  sistec <- x$sistec
  sistec_without_link <- x$sistec_without_link
  qacademico <- x$qacademico
  qacademico_without_link <- x$qacademico_without_link

  # compare student situation
  situation_to_update$Status <- compare_situation_sistec_qacademico(situation_to_update$Status_sistec,
                                                          situation_to_update$Status_q)
  
  # write results
  if(write_output_path != "") {
    path <- paste0(write_output_path, "/", institute)

    write_sistec(sistec_without_cpf, path, "Retificar CPF/Sistec", "sem cpf")
    write_sistec(sistec_without_link, path, "Retificar Curso/Sistec", "curso com erro")
    write_sistec(sistec_without_qacademico, path, "Inserir no Qacademico", "sem qacademico")
    
    write_qacademico(qacademico_without_cpf, path, "Retificar CPF/Qacademico", "sem cpf")
    write_qacademico(qacademico_without_link, path, "Retificar Curso/Qacademico", "curso com erro")
    write_qacademico(qacademico_without_sistec, path, "Inserir no Sistec", "sem sistec")

    write_status_comparison(situation_to_update, path,
                            "Retificar Situa\u00e7\u00e3o", "alterar situa\u00e7\u00e3o") 
  }

  list(sistec_without_cpf = sistec_without_cpf,
       sistec_without_link = sistec_without_link,
       sistec_without_qacademico = sistec_without_qacademico,
       qacademico_without_cpf = qacademico_without_cpf,
       qacademico_without_link = qacademico_without_link,
       qacademico_without_sistec = qacademico_without_sistec,
       situation_to_update = situation_to_update)
}


