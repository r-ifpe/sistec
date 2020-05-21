compare_sistec_rfept <- function(sistec, rfept){

  sistec_complete <- sistec
  rfept_complete <- rfept
  
  # remove invalid cpf
  x <- filter_cpf_sistec(sistec)
  sistec <- x$sistec
  sistec_without_cpf <- x$sistec_without_cpf
  
  x <- filter_cpf_rfept(rfept)
  rfept <- x$rfept
  rfept_without_cpf <- x$rfept_without_cpf

  # remove unlinked cpf
  x <- unlinked_cpf_sistec_rfept(sistec, rfept)
  sistec <- x$sistec
  sistec_without_rfept <- x$sistec_without_rfept
  rfept <- x$rfept
  rfept_without_sistec <- x$rfept_without_sistec
  
  # join rfept and sistec
  situation_to_update <- join_sistec_rfept(sistec, rfept)
  
  # linked courses between rfept and sistec
  situation_to_update <- linked_courses_sistec_rfept(situation_to_update)

  # remove cases unlinked course
  x <- unlinked_course_sistec_rfept(sistec, rfept, situation_to_update)
  sistec <- x$sistec
  sistec_without_rfept <- dplyr::bind_rows(sistec_without_rfept,
                                           x$sistec_without_link)
  rfept <- x$rfept
  rfept_without_sistec <- dplyr::bind_rows(rfept_without_sistec,
                                           x$rfept_without_link)
  
  # compare student situation
  situation_to_update$Status <- compare_situation_sistec_rfept(situation_to_update)
  
  list(sistec_complete = sistec_complete,
       sistec_without_cpf = sistec_without_cpf,
       sistec_without_rfept = sistec_without_rfept,
       rfept_complete = rfept_complete,
       rfept_without_cpf = rfept_without_cpf,
       rfept_without_sistec = rfept_without_sistec,
       situation_to_update = situation_to_update)
}
