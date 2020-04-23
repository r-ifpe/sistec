check_sistec_table <- function(x, expect_nrow){
  expect_equal(colnames(x),
               c("NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA", "NO_STATUS_MATRICULA",
                 "NO_CURSO", "DT_DATA_INICIO", "NO_CAMPUS"))
  
  expect_equal(nrow(x), expect_nrow)
}

check_qacademico_table <- function(x, expect_nrow){
  expect_equal(colnames(x),
               c("Matr\u00edcula", "Nome", "Situa\u00e7\u00e3o.Matr\u00edcula",
                 "Curso", "Cpf", "Institui\u00e7\u00e3o",
                 "Per..Letivo.Inicial", "Campus"))
  
  expect_equal(nrow(x), expect_nrow)
}

download_test_datasets <- function(test_datasets_folder){
  
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
