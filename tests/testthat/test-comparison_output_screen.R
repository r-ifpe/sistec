context("comparison_output_screen")

test_that("comparison_output_screen works ", {
  skip_on_cran()

  sistec_path <- system.file("extdata/test_datasets/sistec", package = "sistec")
  rfept_path <- system.file("extdata/test_datasets/qacademico", package = "sistec")

  rfept <- read_rfept(rfept_path)
  sistec <- read_sistec(sistec_path)
  comparison <- compare_sistec(sistec, rfept)

  # both input paths are NULL
  output_screen <- comparison_output_screen(NULL, NULL, NULL)
  expect_equal(
    "Selecione os arquivos do Sistec e do sistema acad\u00eamico.",
    output_screen
  )

  # only rfept path input is available
  output_screen <- comparison_output_screen(NULL, NULL, rfept_path)
  expect_equal(
    "Selecione os arquivos do sistec.",
    output_screen
  )

  # only sistec path input is available
  output_screen <- comparison_output_screen(NULL, sistec_path, NULL)
  expect_equal(
    "Selecione os arquivos do sistema acad\u00eamico.",
    output_screen
  )  
  
  # both input paths are available and compare button was not clicked yet
  output_screen <- comparison_output_screen(NULL, sistec_path, rfept_path)
  expect_equal(
    "Aperte o bot\u00e3o \"Comparar\" para executar as compara\u00e7\u00f5es.",
    output_screen
  )  
  
  # both input paths are available and compare button was cliked
  output_screen <- comparison_output_screen(comparison, sistec_path, rfept_path)

  expect_true(grepl(
    "Compara\u00e7\u00e3o entre Sistec e Qacademico realizada com sucesso!",
    output_screen
  ))
  expect_true(grepl(
    "Total de alunos:<br/>&emsp; - Sistec: 11099<br/>&emsp; - Qacademico: 14366",
    output_screen
  ))
  expect_true(grepl(
    "Alunos sem CPF:<br/>&emsp; - Sistec: 88<br/>&emsp; - Qacademico: 6",
    output_screen
  ))
  expect_true(grepl(
    "Alunos não encontrados:<br/>&emsp; - Sistec: 528<br/>&emsp; - Qacademico: 3773",
    output_screen
  ))
  expect_true(grepl(
    "CPF's repetidos:<br/>&emsp; - Sistec: 0<br/>&emsp; - Qacademico: 0",
    output_screen
  ))
  expect_true(grepl(
    "Vínculos repetidos:<br/>&emsp; - Sistec: 1<br/>&emsp; - Qacademico: 43",
    output_screen
  ))
  expect_true(grepl(
    "Vínculos não encontrados:<br/>&emsp; - Sistec: 111<br/>&emsp; - Qacademico: 177",
    output_screen
  ))
  expect_true(grepl(
    "Para inspeção manual:<br/>&emsp; - Sistec: 52<br/>&emsp; - Qacademico: 48",
    output_screen
  ))
  expect_true(grepl(
    "Erro no cadastro:<br/>&emsp; - Na data de início: 54",
    output_screen
  ))
  expect_true(grepl(
    "Situações comparadas:<br/>&emsp; - Atualizadas: 9466<br/>&emsp; - Desatualizadas: 799",
    output_screen
  ))
})
