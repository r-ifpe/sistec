context("output_screen")

test_that("compare_screen works", {
  skip_on_cran()
  
  sistec_path <- system.file("extdata/test_datasets/sistec", package = "sistec")
  qacademico_path <- system.file("extdata/test_datasets/qacademico", package = "sistec")
  
  comparison <- compare_sistec(sistec_path, qacademico_path)
  
  output_screen <- sistec:::compare_screen(comparison)
  
  expect_true(grepl("Compara\u00e7\u00e3o entre Sistec e Qacademico realizada com sucesso!",
                    output_screen))
  expect_true(grepl("&emsp; - Sistec: 88<br/>", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 6<br/>", output_screen))
  expect_true(grepl("&emsp; - Sistec: 692<br/>", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 4020", output_screen))
  expect_true(grepl("&emsp; - Atualizadas: 9516", output_screen))
  expect_true(grepl("&emsp; - Desatualizadas: 817", output_screen))
})
