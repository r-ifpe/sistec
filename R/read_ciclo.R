#' Read ciclo files from sistec
#'  
#' This function provides support to read ciclo files that can be extarct from sistec website.
#' 
#' @param path The folder's path to ciclo files. 
#' @param start A character with the date to start the comparison. The default is the minimum
#' value found in the data. The date has to be in this format: "yyyy.semester".
#' Ex.: "2019.1" or "2019.2".
#'
#' @importFrom dplyr %>% 
#' @export
read_ciclo <- function(path = "", start = NULL) {
  if (path == "") stop("You need to specify the path.")
  
  temp <- paste0(path, "/", list.files(path = path, pattern = "*.csv"))
  sep <- detect_sep(temp[1])
  
  vars_ciclo <- c(
    "C\u00d3DIGO CICLO DE MATR\u00cdCULA" , "MUNICIPIO", "CARGA HOR\u00c1RIA TOTAL", 
    "NOME DO CURSO",  "DATA FIM PREVISTO DO CURSO", "DATA IN\u00cdCIO DO CURSO",
    "SUBTIPO CURSOS", "QTD DE MATRICULAS", "QTD DE VAGAS", "QTD DE INSCRITOS",
    "DATA_CRIACAO"
  )
  
  vars_ciclo_file <- names(utils::read.csv(temp[1],
    sep = sep, check.names = FALSE, header = TRUE, encoding = "latin1"
  ))
  
  num_vars_ciclo <- sum(vars_ciclo %in% vars_ciclo_file)

  if (num_vars_ciclo != 11) {
      stop(paste(
        "Not found:", paste(vars_ciclo[!vars_ciclo %in% vars_ciclo_file], collapse = ", ")
      ))
    } else {
      encoding <- detect_encoding(temp[1], sep, "NOME DO CURSO")
      ciclo <- lapply(temp, utils::read.csv,
        sep = sep, stringsAsFactors = FALSE,  encoding = encoding, check.names = FALSE
      ) %>%
        dplyr::bind_rows() %>%
        dplyr::transmute(
          C_CO_CICLO_MATRICULA = !!sym("C\u00d3DIGO CICLO DE MATR\u00cdCULA"),
          C_NO_CAMPUS = !!sym("MUNICIPIO"),
          C_NO_SUBTIPO = !!sym("SUBTIPO CURSOS"),
          C_NU_CARGA_HORARIA = !!sym("CARGA HOR\u00c1RIA TOTAL"),
          C_DT_FIM = substr(!!sym("DATA FIM PREVISTO DO CURSO"), 1, 10),
          C_DT_INICIO = substr(!!sym("DATA IN\u00cdCIO DO CURSO"), 1, 10),
          C_DT_CRIACAO = !!sym("DATA_CRIACAO"),
          C_NU_QTD_MATRICULAS = !!sym("QTD DE MATRICULAS"), 
          C_NU_QTD_VAGAS = !!sym("QTD DE VAGAS"),
          C_NU_QTD_INSCRITOS = !!sym("QTD DE INSCRITOS"),
          C_DT_INICIO_ANO_SEMESTRE = sistec_convert_beginning_date(!!sym("C_DT_INICIO"))
       )
    }
  
  if (!is.null(start)) {
    ciclo <- dplyr::filter(ciclo, !!sym("C_DT_INICIO_ANO_SEMESTRE") >= start)
  }
  
  class(ciclo) <- c("sistec_ciclo_data_frame", class(ciclo))
  ciclo
}
