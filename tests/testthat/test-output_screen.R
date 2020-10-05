context("output_screen")

test_that("compare_screen works", {
  skip_on_cran()
  
  sistec_path <- system.file("extdata/test_datasets/sistec", package = "sistec")
  qacademico_path <- system.file("extdata/test_datasets/qacademico", package = "sistec")
  
  qacademico <- read_rfept(qacademico_path)
  sistec <- read_sistec(sistec_path)

  comparison <- compare_sistec(sistec, qacademico)
  
  output_screen <- sistec:::compare_screen(comparison)

  expect_true(grepl("Compara\u00e7\u00e3o entre Sistec e Qacademico realizada com sucesso!", output_screen)) 
  expect_true(grepl("Total de alunos:<br/>&emsp; - Sistec: 11099<br/>&emsp; - Qacademico: 14366", output_screen))
  expect_true(grepl("Alunos sem CPF:<br/>&emsp; - Sistec: 88<br/>&emsp; - Qacademico: 6", output_screen))
  expect_true(grepl("CPF's repetidos:<br/>&emsp; - Sistec: 0<br/>&emsp; - Qacademico: 0", output_screen))
  expect_true(grepl("Vínculos repetidos:<br/>&emsp; - Sistec: 2<br/>&emsp; - Qacademico: 67", output_screen))
  expect_true(grepl("Vínculos não encontrados:<br/>&emsp; - Sistec: 639<br/>&emsp; - Qacademico: 3927", output_screen))
  expect_true(grepl("Para inspeção manual:<br/>&emsp; - Sistec: 52<br/>&emsp; - Qacademico: 48", output_screen))
  expect_true(grepl("Erro no cadastro:<br/>&emsp; - Na data de início: 54", output_screen)) 
  expect_true(grepl("Situações comparadas:<br/>&emsp; - Atualizadas: 9465<br/>&emsp; - Desatualizadas: 799", output_screen))
})
