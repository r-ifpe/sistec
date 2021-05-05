#' @importFrom dplyr %>% 
#' @export
read_sistec_students <- function(path = "") {
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
        S_NO_CICLO_MATRICULA = !!sym("NO_CICLO_MATRICULA")
      )
  }
  
  class(students) <- c("sistec_students_data_frame", class(students))
  students
}

