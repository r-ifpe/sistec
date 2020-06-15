check_sistec_table <- function(x, expect_nrow){
  expect_equal(colnames(x),
               c("S_NO_ALUNO", "S_NU_CPF", "S_CO_CICLO_MATRICULA", "S_NO_STATUS_MATRICULA",
                 "S_NO_CURSO", "S_DT_INICIO_CURSO", "S_NO_CAMPUS"))
  
  expect_equal(nrow(x), expect_nrow)
}

check_rfept_table <- function(x, expect_nrow){
  expect_equal(colnames(x),
               c("R_NO_ALUNO", "R_NU_CPF", "R_CO_MATRICULA", "R_CO_CICLO_MATRICULA",
                 "R_NO_STATUS_MATRICULA", "R_NO_CURSO", "R_DT_INICIO_CURSO", 
                 "R_NO_CAMPUS", "R_NO_COTA"))
  
  expect_equal(nrow(x), expect_nrow)
}

check_rfept_wrong_registration <- function(x, expect_nrow){
  expect_equal(colnames(x),
               c("R_NO_CURSO", "S_NO_CURSO_LINKED", "S_QT_ALUNOS_LINKED", "R_NO_ALUNO",
                 "R_NU_CPF", "R_CO_MATRICULA", "R_CO_CICLO_MATRICULA", "R_NO_STATUS_MATRICULA",
                 "R_DT_INICIO_CURSO", "R_NO_CAMPUS", "R_NO_COTA", "S_NO_ALUNO",           
                 "S_CO_CICLO_MATRICULA", "S_NO_STATUS_MATRICULA", "S_NO_CURSO",           
                 "S_DT_INICIO_CURSO", "S_NO_CAMPUS"))
  
  expect_equal(nrow(x), expect_nrow)
}

check_situation_table <- function(x, expect_nrow){
  expect_equal(colnames(x),
               c("R_NO_CURSO", "S_NO_CURSO_LINKED", "S_QT_ALUNOS_LINKED", "S_NO_ALUNO",           
                 "S_NU_CPF", "S_CO_CICLO_MATRICULA", "S_NO_STATUS_MATRICULA", "S_NO_CURSO",           
                 "S_DT_INICIO_CURSO", "S_NO_CAMPUS", "R_NO_ALUNO",  "R_CO_MATRICULA",       
                 "R_CO_CICLO_MATRICULA", "R_NO_STATUS_MATRICULA", "R_DT_INICIO_CURSO",
                 "R_NO_CAMPUS", "R_NO_COTA", "S_NO_STATUS_IGUAL" ))
  
  expect_equal(nrow(x), expect_nrow)
}

check_linked_courses_table <- function(x, expect_nrow){
  expect_equal(colnames(x),
               c("R_DT_INICIO_CURSO", "R_NO_CURSO", "R_NO_CAMPUS",
                 "S_NO_CURSO_LINKED", "S_CO_CICLO_MATRICULA"))
  
  expect_equal(nrow(x), expect_nrow)
}

download_test_datasets <- function(test_datasets_folder = NULL){
  
  if(is.null(test_datasets_folder)){
    stop("You need to specify a path.")
  }
  
  if(system.file(test_datasets_folder, package = "sistec") == ""){
    
    datasets_path <- paste0(system.file(package = "sistec"), "/",
                            test_datasets_folder, "/")
    
    dir.create(paste0(datasets_path, "qacademico/"), recursive = TRUE)
    dir.create(paste0(datasets_path, "sistec/"), recursive = TRUE)
    
    destfile <- c(paste0(datasets_path, "qacademico/fake_data_qacademico_2019_1.csv"),
                  paste0(datasets_path, "qacademico/fake_data_qacademico_2019_2.csv"),
                  paste0(datasets_path, "qacademico/fake_data_qacademico_2020_1.csv"),
                  paste0(datasets_path, "sistec/fake_data_sistec_2019.1_2020.1.csv"))
    
    
    datasests_paths <- c("https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/qacademico/fake_data_qacademico_2019_1.csv",
                         "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/qacademico/fake_data_qacademico_2019_2.csv",
                         "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/qacademico/fake_data_qacademico_2020_1.csv",
                         "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/sistec/fake_data_sistec_2019.1_2020.1.csv")
    
    for(i in 1:4){
      utils::download.file(datasests_paths[i], 
                           destfile = destfile[i],
                           # method = "wget",
                           quiet = TRUE)
    }
  }
}
