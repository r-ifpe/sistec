#' Read students
#'  
#' This function provides support to read sistec students files that can 
#' be extarct from sistec website.
#' 
#' @param path The folder's path to students files.
#' @param start A character with the date to start the comparison. The default is the minimum
#' value found in the data. The date has to be in this format: "yyyy.semester".
#' Ex.: "2019.1" or "2019.2".
#'
#' @importFrom dplyr %>% 
#' @export
read_sistec_students <- function(path = "", start = NULL) {
  if (path == "") stop("You need to specify the path.")

  temp <- paste0(path, "/", list.files(path = path, pattern = "*.csv"))
  sep <- detect_sep(temp[1])
  
  vars_students <- c(
    "NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA",
    "NO_STATUS_MATRICULA", "NO_CICLO_MATRICULA"
  )
  
  vars_students_file <- names(utils::read.csv(temp[1],
    sep = sep, check.names = FALSE, header = TRUE, encoding = "UTF-8"
  ))
  
  num_vars_students <- sum(vars_students %in% vars_students_file)

  if (num_vars_students != 5) {
    stop(paste(
      "Not found:",
      paste(vars_students[!vars_students %in% vars_students_file], collapse = ", ")
    ))
  } else {
    encoding <- detect_encoding(temp[1], sep, "NO_CICLO_MATRICULA")
    students <- lapply(temp, utils::read.csv,
      sep = sep, stringsAsFactors = FALSE,  encoding = encoding
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::transmute(
        S_CO_CICLO_MATRICULA = !!sym("CO_CICLO_MATRICULA"),
        S_NO_ALUNO = !!sym("NO_ALUNO"),
        S_NU_CPF = !!sym("NU_CPF"),
        S_NO_STATUS_MATRICULA = !!sym("NO_STATUS_MATRICULA"),
        S_NO_CICLO_MATRICULA = !!sym("NO_CICLO_MATRICULA"),
        S_DT_INICIO_ANO_SEMESTRE = sistec_convert_beginning_date(!!sym("DT_DATA_INICIO"))
      )
  }
  
 if (!is.null(start)) {
   students <- dplyr::filter(students, !!sym("S_DT_INICIO_ANO_SEMESTRE") >= start)
 }
  
  class(students) <- c("sistec_students_data_frame", class(students))
  students
}

