context("output_screen")

test_that("compare_screen works", {
  skip_on_cran()
  
  sistec_path <- system.file("extdata/test_datasets/sistec", package = "sistec")
  qacademico_path <- system.file("extdata/test_datasets/qacademico", package = "sistec")
  
  qacademico <- read_rfept(qacademico_path)
  sistec <- read_sistec(sistec_path)
  
  comparison <- compare_sistec(sistec, qacademico)
  
  output_screen <- sistec:::compare_screen(comparison)

  expect_true(grepl("Compara\u00e7\u00e3o entre Sistec e Qacademico realizada com sucesso!",
                    output_screen))
  expect_true(grepl("&emsp; - Sistec: 88<br/>", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 6<br/>", output_screen))
  expect_true(grepl("&emsp; - Sistec: 648<br/>", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 3975", output_screen))
  expect_true(grepl("&emsp; - Na data de início: 56", output_screen))
  expect_true(grepl("&emsp; - No ciclo: 0", output_screen))
  expect_true(grepl("&emsp; - Atualizadas: 9505", output_screen))
  expect_true(grepl("&emsp; - Desatualizadas: 817", output_screen))
})
