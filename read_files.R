#' @export
read_sistec <- function(path = "extdata"){
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)
  sistec <- lapply(temp, utils::read.csv,
                   sep = ";",  stringsAsFactors=FALSE, colClasses =rep("character", 8)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(NO_STATUS_MATRICULA = ifelse(
      stringr::str_detect(NO_STATUS_MATRICULA, "CONCLU\xcdDA"), "CONCLUÍDA", NO_STATUS_MATRICULA),
      NU_CPF = num_para_cpf(NU_CPF)) %>%
    dplyr::select(NO_ALUNO, NU_CPF, CO_CICLO_MATRICULA, NO_STATUS_MATRICULA)
}

#' @export
read_qacademico <- function(path = "extdata"){
  temp = list.files(path = path, pattern = "*.xlsx")
  temp <- paste0(path , "/", temp)
  lapply(temp, openxlsx::read.xlsx) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Cpf= num_para_cpf(Cpf))
}

#' @export
server_input_path <- function(input_path){
  slash <- stringr::str_locate_all(input_path[1], "/")
  last_slash <- slash[[1]][nrow(slash[[1]]), 2]
  substr(input_path[1], 1, last_slash)
}