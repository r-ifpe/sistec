context("read_generic_rfept")

 test_that("read_generic_rfept", {
  skip_on_cran()
  
  rfept <- read_generic_rfept(
    system.file("extdata/test_datasets/generic_rfept/rfept",
                package = "sistec"))
  
  check_rfept_table(rfept, expect_nrow = 45)
  expect_true(inherits(rfept, "generic_rfept_table"))
}) 

test_that("detect_sep works", {
  skip_on_cran()
  
  expect_error(read_generic_rfept(
    system.file("extdata/test_datasets/generic_rfept/wrong_separator",
                package = "sistec")),
    "Separador diferente de , ou ;")
}) 

test_that("check_rfept_header works", {
  skip_on_cran()
  
 expect_error(read_generic_rfept(
    system.file("extdata/test_datasets/generic_rfept/wrong_header",
                package = "sistec")),
    "Sistema acad\u00eamico: As colunas NU_CPF, DT_INICIO_CURSO est\u00e3o faltando.")
}) 

test_that("check_rfept_cpf works", {
  skip_on_cran()
  
  expect_error(read_generic_rfept(
    system.file("extdata/test_datasets/generic_rfept/wrong_cpf",
                package = "sistec")),
    "Sistema acad\u00eamico: Os CPF's n\u00e3o est\u00e3o no formato xxx.xxx.xxx-xx.")
}) 

test_that("check_rfept_course_beginning_date works", {
  skip_on_cran()
  
  expect_error(read_generic_rfept(
    system.file("extdata/test_datasets/generic_rfept/wrong_beginning_date",
                package = "sistec")),
    paste("Sistema acad\u00eamico: A data de in\u00edcio n\u00e3o est\u00e1",
          "no formato aaaa.s. Ex.: 2020.1.",
          "Verifique também se o período está correto: 1 para o primeiro",
          "semestre e 2 para o segundo."))
}) 

test_that("check_rfept_status works", {
  skip_on_cran()
  
  expect_error(read_generic_rfept(
    system.file("extdata/test_datasets/generic_rfept/wrong_status",
                package = "sistec")),
    paste("Sistema acad\u00eamico: Os status dos alunos n\u00e3o est\u00e3o",
          "no formato do Sistec.",
          "Utilize: ABANDONO, CONCLUÍDA, DESLIGADO, EM_CURSO,",
          "INTEGRALIZADA, REPROVADA ou TRANSF_EXT"))
})



