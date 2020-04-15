context("read_files")

test_that("read_sistec works", {
  skip_on_cran()
  
  sistec <- sistec::read_sistec(system.file("extdata/test_datasets/sistec",
                                            package = "sistec"))
  
  expect_equal(nrow(sistec), 11099)
  expect_equal(ncol(sistec), 7)
  expect_equal(names(sistec), 
               c("NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA", "NO_STATUS_MATRICULA",
                 "NO_CURSO", "DT_DATA_INICIO", "NO_CAMPUS"))
})

test_that("read_qacademico works", {
  skip_on_cran()
  
  qacademico <- sistec::read_qacademico(system.file("extdata/test_datasets/qacademico",
                                                    package = "sistec"))

  expect_equal(nrow(qacademico), 14366)
  expect_equal(ncol(qacademico), 8)
  expect_equal(names(qacademico), 
               c("Matr\u00edcula", "Nome", "Situa\u00e7\u00e3o.Matr\u00edcula",
                 "Curso", "Cpf", "Institui\u00e7\u00e3o",
                 "Per..Letivo.Inicial", "Campus")) 
})

