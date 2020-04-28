#' Read Qacademico files
#'
#' This function support two kinds of schemas: from the api and the website. See Details 
#' if you need help to download the Qacademico data.
#'
#' @param path The Qacademico file's path. 
#' @return A data frame.
#' 
#' @details To download the student's data, go to your proper account on Qacademico and 
#' follow:
#'  
#' - "Relatorio de Alunos" -> "Listagem de Alunos" (choose year and period)
#' - Click on "visualizar" 
#' - Using F10 shortcut and save in .csv format
#' - Rename the including year and period (example2020_1.csv) 
#'
#' Be sure that your data has the variables: "Matricula", "Nome", "Situacao Matricula", 
#' "Curso", "Cpf", "Instituicao", "Per. Letivo Inicial".
#' 
#' @examples  
#' # this dataset is not a real one. It is just for test purpose.
#' qacademico <- read_qacademico(system.file("extdata/examples/qacademico",
#'                                           package = "sistec"))
#' 
#' qacademico
#' @importFrom rlang sym
#' @export
read_qacademico <- function(path = ""){
  
  if(path == "") stop("You need to specify the path.")
  
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  qacademico <- utils::read.csv(temp[1], sep = "",  stringsAsFactors = FALSE, 
                                encoding = "latin1", nrows = 1, check.names = FALSE)
  
  vars <- c("Matr\u00edcula", "Nome", "Situa\u00e7\u00e3o Matr\u00edcula",
            "Curso", "Cpf", "Institui\u00e7\u00e3o", "Per. Letivo Inicial")
  
  if(sum(names(qacademico) %in% vars) == 7){
    read_qacademico_web(path)
  } else{
    stop(paste("Not found:",
               paste(vars[!vars %in% names(qacademico)], collapse = ", ")))
  }
}

#' @importFrom rlang sym syms
read_qacademico_web <- function(path){
  temp <-  list.files(path = path, pattern = "*.csv")
  temp <- paste0(path , "/", temp) %>% sort(decreasing = TRUE)
  
  # Matrícula, Situação.Matrícula, Situação.Período, Instituição
  vars <- c("Matr\u00edcula", "Nome", "Situa\u00e7\u00e3o.Matr\u00edcula",
            "Curso", "Cpf", "Institui\u00e7\u00e3o", "Per..Letivo.Inicial")
  
  classes <- c(Cpf = "character")
  
  qacademico <- lapply(temp, function(e){
    utils::read.csv(e, sep = "",  stringsAsFactors = FALSE, 
                    encoding = "latin1", colClasses = classes) %>% 
        dplyr::select(!!!syms(vars))
    }) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(Cpf = num_para_cpf(!!sym("Cpf")),
                  Campus = stringr::str_sub(!!sym("Institui\u00e7\u00e3o"),8)) %>% 
    dplyr::distinct(!!sym("Matr\u00edcula"), .keep_all = TRUE) %>% # I found this problem 
    dplyr::mutate(Campus = ifelse(!!sym("Campus") == "", "SEM CAMPUS", !!sym("Campus"))) 
  
  class(qacademico) <- c(class(qacademico), "qacademico_data_frame")

  qacademico
}
