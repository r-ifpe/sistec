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
    
  expect_true(grepl("Total de alunos:", output_screen))
  expect_true(grepl("&emsp; - Sistec: 11099", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 14366", output_screen))
  
  expect_true(grepl("Alunos sem CPF:", output_screen))
  expect_true(grepl("&emsp; - Sistec: 88<br/>", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 6<br/>", output_screen))
 
  expect_true(grepl("CPF's repetidos:", output_screen))
  expect_true(grepl("&emsp; - Sistec: 0<br/>", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 0<br/>", output_screen)) 
  
  expect_true(grepl("Vínculos repetidos:", output_screen))
  expect_true(grepl("&emsp; - Sistec: 2<br/>", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 67<br/>", output_screen)) 
  
  expect_true(grepl("Vínculos não encontrados:", output_screen))
  expect_true(grepl("&emsp; - Sistec: 639<br/>", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 3927", output_screen))

  expect_true(grepl("Para inspeção manual:", output_screen))
  expect_true(grepl("&emsp; - Sistec: 2<br/>", output_screen))
  expect_true(grepl("&emsp; - Qacademico: 67", output_screen))
    
  expect_true(grepl("Erro no cadastro:", output_screen)) 
  expect_true(grepl("&emsp; - Na data de início: 54", output_screen))
  
  expect_true(grepl("Situações comparadas:", output_screen))
  expect_true(grepl("&emsp; - Atualizadas: 9465", output_screen))
  expect_true(grepl("&emsp; - Desatualizadas: 799", output_screen))
})
