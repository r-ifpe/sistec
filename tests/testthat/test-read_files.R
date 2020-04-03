context("read_files")

test_that("read_sistec works", {
  skip_on_cran()
  
  sistec <- sistec::read_sistec(system.file("extdata/sistec", package = "sistec"),
                                type = "simplified")
  
  expect_equal(nrow(sistec), 1017)
  expect_equal(ncol(sistec), 4)
  expect_equal(names(sistec), 
               c("NO_ALUNO", "NU_CPF", "CO_CICLO_MATRICULA", "NO_STATUS_MATRICULA"))
})

test_that("read_qacademico works", {
  skip_on_cran()
  
  qacademico <- sistec::read_qacademico(system.file("extdata/qacademico", package = "sistec"),
                                        type = "simplified")

  expect_equal(nrow(qacademico), 936)
  expect_equal(ncol(qacademico), 3)
  expect_equal(names(qacademico), 
               c("Nome", "Situa\u00e7\u00e3o.Matr\u00edcula", "Cpf")) #   Situação.Matrícula
})

