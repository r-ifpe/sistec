test_that("read_sistec_students works", {
  skip_on_cran()
  
  students <- read_sistec_students(
    system.file(
      "extdata/test_datasets/pnp_critics/students", package = "sistec"
    ))
  
  expect_equal(nrow(students), 3062)
  expect_equal(
    names(students),
    c(
      "S_CO_CICLO_MATRICULA", "S_NO_ALUNO", "S_NU_CPF",                
      "S_NO_STATUS_MATRICULA", "S_NO_CICLO_MATRICULA", "S_DT_INICIO_ANO_SEMESTRE"
    ))
  
  students <- read_sistec_students(
    system.file(
      "extdata/test_datasets/pnp_critics/students", package = "sistec"
    ), start = "2020.1")
  
  
  expect_equal(nrow(students), 460)
  expect_equal(
    names(students),
    c(
      "S_CO_CICLO_MATRICULA", "S_NO_ALUNO", "S_NU_CPF",                
      "S_NO_STATUS_MATRICULA", "S_NO_CICLO_MATRICULA", "S_DT_INICIO_ANO_SEMESTRE"
    ))
})