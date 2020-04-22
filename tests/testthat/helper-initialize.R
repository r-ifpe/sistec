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


