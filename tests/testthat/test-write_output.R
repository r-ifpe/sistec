context("write_output")

test_that("write_output() works", {
  skip_on_cran()

  sistec_path <- system.file("extdata/examples/sistec", package = "sistec")
  rfept_path <- system.file("extdata/examples/qacademico", package = "sistec")

  sistec <- dplyr::filter(
    read_sistec(sistec_path),
    grepl("SANEAMENTO", S_NO_CURSO)
  )

  rfept <- dplyr::filter(
    read_rfept(rfept_path),
    grepl("SANEAMENTO", R_NO_CURSO)
  )

  comparison <- compare_sistec(sistec, rfept)
  temp_folder <- tempdir()
  write_output(comparison, temp_folder)

  aria_files_path <- c(paste0(
    temp_folder, "/ARIA/",
    c(
      "Cadastrar Alunos/RECIFE/CURSO TEC EM SANEAMENTO INTEG_ REG RC 2014_1/2019.2/Cadastrar alunos.xlsx",
      "Cadastrar Alunos/RECIFE/CURSO T\u00c9CNICO EM SANEAMENTO SUB 2014_1/2019.1/Cadastrar alunos.xlsx",
      "Cadastrar Alunos/RECIFE/CURSO T\u00c9CNICO EM SANEAMENTO SUB 2014_1/2019.2/Cadastrar alunos.xlsx",
      "Cursos Relacionados/Cursos relacionados.xlsx",
      "Retificar no Qacademico/RECIFE/CURSO TEC EM SANEAMENTO INTEG_ REG RC 2014_1/Qacademico.xlsx",
      "Retificar no Qacademico/RECIFE/CURSO T\u00c9CNICO EM SANEAMENTO SUB 2014_1/Qacademico.xlsx",
      "Retificar no Sistec/RECIFE/T\u00c9CNICO EM SANEAMENTO/Sistec.xlsx",
      "Tabelas Utilizadas/Tabelas Utilizadas.xlsx"
    )
  ))

  aria_files_by_course <- lapply(aria_files_path[1:7], openxlsx::read.xlsx)
  aria_file_all_tables <- lapply(1:6, function(e) {
    openxlsx::read.xlsx(aria_files_path[8], sheet = e)
  })

  # check names -------------------------------------------------------------------
  expect_equal(
    unlist(lapply(aria_files_by_course, names)),
    c(
      "NOME", "CPF", "MATRICULA", "COTA",
      "NOME", "CPF", "MATRICULA", "COTA",
      "NOME", "CPF", "MATRICULA", "COTA",
      "INICIO", "CICLO", "CURSO_SISTEC", "CURSO_QACADEMICO", "CAMPUS",
      "NOME", "CPF", "MATRICULA", "INICIO",
      "NOME", "CPF", "MATRICULA", "INICIO",
      "NOME", "CPF", "CICLO", "INICIO"
    )
  )

  expect_equal(
    unlist(lapply(aria_file_all_tables, names)),
    c(
      "NOME", "CPF", "CICLO", "INICIO", "CAMPUS", "CURSO",
      "NOME", "CPF", "MATRICULA", "INICIO", "CAMPUS", "CURSO",
      "NOME", "CPF", "MATRICULA", "INICIO", "CAMPUS", "CURSO",
      "NOME", "CPF", "MATRICULA_QACADEMICO", "STATUS_QACADEMICO", "STATUS_SISTEC", "CAMPUS", "CURSO", "CICLO",
      "NOME", "CPF", "MATRICULA_QACADEMICO", "STATUS_QACADEMICO", "STATUS_SISTEC", "CAMPUS", "CURSO", "CICLO",
      "INICIO", "CICLO", "CURSO_SISTEC", "CURSO_QACADEMICO", "CAMPUS"
    )
  )

  # check row  --------------------------------------------------------
  expect_equal(
    unlist(lapply(aria_files_by_course, nrow)),
    c(2, 4, 1, 2, 2, 5, 5)
  )

  expect_equal(
    unlist(lapply(aria_file_all_tables, nrow)),
    c(5, 7, 5, 9, 2, 2)
  )
})

test_that("write_output() deletes an existed ARIA folder", {
  skip_on_cran()

  sistec_path <- system.file("extdata/examples/sistec", package = "sistec")
  rfept_path <- system.file("extdata/examples/qacademico", package = "sistec")

  # with the course SANEAMENTO
  sistec <- dplyr::filter(
    read_sistec(sistec_path),
    grepl("TRABALHO|SANEAMENTO", S_NO_CURSO)
  )

  rfept <- dplyr::filter(
    read_rfept(rfept_path),
    grepl("TRABALHO|SANEAMENTO", R_NO_CURSO)
  )

  comparison <- compare_sistec(sistec, rfept)
  temp_folder <- tempdir()
  write_output(comparison, temp_folder)

  expect_true(dir.exists(paste0(
    temp_folder,
    "/ARIA/Cadastrar Alunos/RECIFE/CURSO T\u00c9CNICO EM SANEAMENTO SUB 2014_1/2019.1"
  )))

  # without the course SANEAMENTO
  sistec <- dplyr::filter(
    read_sistec(sistec_path),
    grepl("TRABALHO", S_NO_CURSO)
  )

  rfept <- dplyr::filter(
    read_rfept(rfept_path),
    grepl("TRABALHO", R_NO_CURSO)
  )

  comparison <- compare_sistec(sistec, rfept)
  temp_folder <- tempdir()
  write_output(comparison, temp_folder)

  expect_false(dir.exists(paste0(
    temp_folder,
    "/ARIA/Cadastrar Alunos/RECIFE/CURSO T\u00c9CNICO EM SANEAMENTO SUB 2014_1/2019.1"
  )))
})
