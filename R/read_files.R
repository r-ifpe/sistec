#' @export
read_sistec <- function(path = "extdata"){
  temp = list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp)

  # read just the first row to infer the schema
  sistec <- utils::read.csv(temp[1], sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

  # there are two kinds of sistec schema
  if (any(grepl("Numero.Cpf", names(sistec)))){
    sistec_schema_1(temp)
  } else {
    sistec_schema_2(temp)
  }
}

#' @export
read_qacademico <- function(path = "extdata"){
  temp = list.files(path = path, pattern = "*.xlsx")
  temp <- paste0(path , "/", temp)
  lapply(temp, openxlsx::read.xlsx) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Cpf= num_para_cpf(Cpf))
}


server_input_path <- function(input_path){
  slash <- stringr::str_locate_all(input_path[1], "/")
  last_slash <- slash[[1]][nrow(slash[[1]]), 2]
  substr(input_path[1], 1, last_slash)
}

sistec_schema_1 <- function(temp){
  classes <- c(Numero.Cpf = "character")
  
  lapply(temp, utils::read.csv,
                   sep = ";",  stringsAsFactors = FALSE, colClasses = classes, encoding = "UTF-8") %>%
    dplyr::bind_rows() %>%
    dplyr::transmute(NO_ALUNO = Nome.Aluno, NU_CPF = num_para_cpf(Numero.Cpf),
                     CO_CICLO_MATRICULA = Co.Ciclo.Matricula, 
                     NO_STATUS_MATRICULA = `Situação.Matricula`,
                     NO_CURSO = No.Curso, DT_DATA_INICIO = Dt.Data.Inicio)
}

sistec_schema_2 <- function(temp){
  classes <- c(NU_CPF = "character", CO_CICLO_MATRICULA = "character",
               NO_STATUS_MATRICULA = "character")
  
  lapply(temp, utils::read.csv,
                   sep = ";",  stringsAsFactors = FALSE, colClasses = classes, encoding = "latin1") %>%
    dplyr::bind_rows() %>%
    dplyr::transmute(NO_ALUNO, NU_CPF = num_para_cpf(NU_CPF), 
                     CO_CICLO_MATRICULA, NO_STATUS_MATRICULA)
}
