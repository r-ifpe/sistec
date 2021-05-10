check_sistec_table <- function(x, expect_nrow) {
  expect_equal(
    colnames(x),
    c(
      "S_NO_ALUNO", "S_NU_CPF", "S_CO_CICLO_MATRICULA", "S_NO_STATUS_MATRICULA",
      "S_NO_CURSO", "S_DT_INICIO_CURSO", "S_NO_CAMPUS"
    )
  )

  expect_equal(nrow(x), expect_nrow)
}

check_rfept_table <- function(x, expect_nrow) {
  expect_equal(
    colnames(x),
    c(
      "R_NO_ALUNO", "R_NU_CPF", "R_CO_MATRICULA", "R_CO_CICLO_MATRICULA",
      "R_NO_STATUS_MATRICULA", "R_NO_CURSO", "R_DT_INICIO_CURSO",
      "R_NO_CAMPUS", "R_NO_COTA"
    )
  )

  expect_equal(nrow(x), expect_nrow)
}

check_sistec_students_table <- function(x, expect_nrow) {
  expect_equal(
    colnames(x),
    c(
      "S_CO_CICLO_MATRICULA", "S_NO_ALUNO", "S_NU_CPF",                
      "S_NO_STATUS_MATRICULA", "S_NO_CICLO_MATRICULA", "S_DT_INICIO_ANO_SEMESTRE"
    )
  )
  
  expect_equal(nrow(x), expect_nrow)
  expect_true(inherits(x, "sistec_students_data_frame"))
}

check_ciclo_table <- function(x, expect_nrow) {
  expect_equal(
    colnames(x),
    c(
      "C_CO_CICLO_MATRICULA", "C_NO_CAMPUS", "C_NO_SUBTIPO", "C_NU_CARGA_HORARIA",
      "C_DT_FIM", "C_DT_INICIO", "C_DT_CRIACAO", "C_NU_QTD_MATRICULAS", "C_NU_QTD_VAGAS",
      "C_NU_QTD_INSCRITOS", "C_DT_INICIO_ANO_SEMESTRE"
    )
  )
  
  expect_equal(nrow(x), expect_nrow)
  expect_true(inherits(x, "sistec_ciclo_data_frame"))
}

check_wrong_registration <- function(x, expect_nrow) {
  expect_equal(
    colnames(x),
    c(
      "R_NO_CURSO", "S_NO_CURSO_LINKED", "S_QT_ALUNOS_LINKED", "R_NO_ALUNO",
      "R_NU_CPF", "R_CO_MATRICULA", "R_CO_CICLO_MATRICULA", "R_NO_STATUS_MATRICULA",
      "R_DT_INICIO_CURSO", "R_NO_CAMPUS", "R_NO_COTA", "S_NO_ALUNO",
      "S_CO_CICLO_MATRICULA", "S_NO_STATUS_MATRICULA", "S_NO_CURSO",
      "S_DT_INICIO_CURSO", "S_NO_CAMPUS"
    )
  )

  expect_equal(nrow(x), expect_nrow)
}

check_situation_table <- function(x, expect_nrow) {
  expect_true(all(colnames(x) %in%
    c(
      "R_NO_CURSO", "S_NO_CURSO_LINKED", "S_QT_ALUNOS_LINKED", "S_NO_ALUNO",
      "S_NU_CPF", "S_CO_CICLO_MATRICULA", "S_NO_STATUS_MATRICULA", "S_NO_CURSO",
      "S_DT_INICIO_CURSO", "S_NO_CAMPUS", "R_NO_ALUNO", "R_CO_MATRICULA",
      "R_CO_CICLO_MATRICULA", "R_NO_STATUS_MATRICULA", "R_DT_INICIO_CURSO",
      "R_NO_CAMPUS", "R_NO_COTA", "S_NO_STATUS_IGUAL"
    )))

  expect_equal(nrow(x), expect_nrow)
}

check_linked_courses_table <- function(x, expect_nrow) {
  expect_equal(
    colnames(x),
    c(
      "R_DT_INICIO_CURSO", "R_NO_CURSO", "R_NO_CAMPUS",
      "S_NO_CURSO_LINKED", "S_CO_CICLO_MATRICULA"
    )
  )

  expect_equal(nrow(x), expect_nrow)
}

check_comparison <- function(x, rows) {
  expect_equal(
    names(x),
    c(
      "sistec_complete", "sistec_without_cpf", "sistec_without_rfept", "sistec_wrong_cpf",
      "sistec_duplicated_registry", "sistec_unlinked_entry", "sistec_pending",
      "rfept_complete", "rfept_without_cpf", "rfept_without_sistec", "rfept_wrong_cpf",
      "rfept_duplicated_registry", "rfept_unlinked_entry", "rfept_pending",
      "wrong_beginning", "situation_updated", "situation_to_update", "linked_courses"
    )
  )

  check_sistec_table(x$sistec_complete, expect_nrow = rows[1])
  check_sistec_table(x$sistec_without_cpf, expect_nrow = rows[2])
  check_sistec_table(x$sistec_without_rfept, expect_nrow = rows[3])
  check_sistec_table(x$sistec_wrong_cpf, expect_nrow = rows[4])
  check_sistec_table(x$sistec_duplicated_registry, expect_nrow = rows[5])
  check_sistec_table(x$sistec_unlinked_entry, expect_nrow = rows[6])
  check_sistec_table(x$sistec_pending, expect_nrow = rows[7])

  check_rfept_table(x$rfept_complete, expect_nrow = rows[8])
  check_rfept_table(x$rfept_without_cpf, expect_nrow = rows[9])
  check_rfept_table(x$rfept_without_sistec, expect_nrow = rows[10])
  check_rfept_table(x$rfept_wrong_cpf, expect_nrow = rows[11])
  check_rfept_table(x$rfept_duplicated_registry, expect_nrow = rows[12])
  check_rfept_table(x$rfept_unlinked_entry, expect_nrow = rows[13])
  check_rfept_table(x$rfept_pending, expect_nrow = rows[14])

  check_wrong_registration(x$wrong_beginning, expect_nrow = rows[15])
  check_situation_table(x$situation_updated, expect_nrow = rows[16])
  check_situation_table(x$situation_to_update, expect_nrow = rows[17])
  check_linked_courses_table(x$linked_courses, expect_nrow = rows[18])
  check_amount_entry(x)
}

check_amount_entry <- function(x) {
  amount <- unlist(lapply(x, nrow))

  expect_true(amount[1] == sum(amount[c(2, 3, 4, 5, 6, 7, 15, 16, 17)]))
  expect_true(amount[8] == sum(amount[c(9, 10, 11, 12, 13, 14, 15, 16, 17)]))
}

download_test_datasets <- function(test_datasets_folder = NULL) {
  if (is.null(test_datasets_folder)) {
    stop("You need to specify a path.")
  }

  if (system.file(test_datasets_folder, package = "sistec") == "") {
    datasets_path <- paste0(
      system.file(package = "sistec"), "/",
      test_datasets_folder, "/"
    )

    dir_test <- c(
      "qacademico/", "sistec/",
      "sistec_encoding/latin1/", "sistec_encoding/utf8/",
      "generic_rfept/rfept/", "generic_rfept/sistec/",
      "generic_rfept/wrong_beginning_date/", "generic_rfept/wrong_cota/",
      "generic_rfept/wrong_cpf/", "generic_rfept/wrong_header/",
      "generic_rfept/wrong_separator/", "generic_rfept/wrong_status/"
    )

    invisible(lapply(
      paste0(datasets_path, dir_test),
      dir.create,
      recursive = TRUE
    ))

    test_files <- c(
      "qacademico/fake_data_qacademico_2019_1.csv",
      "qacademico/fake_data_qacademico_2019_2.csv",
      "qacademico/fake_data_qacademico_2020_1.csv",
      "sistec/fake_data_sistec_2019.1_2020.1.csv",
      "sistec_encoding/latin1/fake_data_sistec_latin1.csv",
      "sistec_encoding/utf8/fake_data_sistec_utf8.csv",
      "generic_rfept/rfept/rfept.csv",
      "generic_rfept/sistec/sistec.csv",
      "generic_rfept/wrong_beginning_date/fake_generic_rfept.csv",
      "generic_rfept/wrong_cota/fake_generic_rfept.csv",
      "generic_rfept/wrong_cpf/fake_generic_rfept.csv",
      "generic_rfept/wrong_header/fake_generic_rfept.csv",
      "generic_rfept/wrong_separator/fake_generic_rfept.csv",
      "generic_rfept/wrong_status/fake_generic_rfept.csv"
    )

    destfile <- paste0(datasets_path, test_files)

    datasests_paths <- c(
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/qacademico/fake_data_qacademico_2019_1.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/qacademico/fake_data_qacademico_2019_2.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/qacademico/fake_data_qacademico_2020_1.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/sistec/fake_data_sistec_2019.1_2020.1.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/sistec_encoding/latin1/fake_data_sistec_latin1.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/sistec_encoding/utf8/fake_data_sistec_utf8.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/generic_rfept/rfept/rfept.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/generic_rfept/sistec/sistec.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/generic_rfept/wrong_beginning_date/fake_generic_rfept.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/generic_rfept/wrong_cota/fake_generic_rfept.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/generic_rfept/wrong_cpf/fake_generic_rfept.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/generic_rfept/wrong_header/fake_generic_rfept.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/generic_rfept/wrong_separator/fake_generic_rfept.csv",
      "https://raw.githubusercontent.com/r-ifpe/sistec/master/inst/extdata/test_datasets/generic_rfept/wrong_status/fake_generic_rfept.csv"
    )

    for (i in 1:length(destfile)) {
      utils::download.file(datasests_paths[i],
        destfile = destfile[i],
        # method = "wget",
        quiet = TRUE
      )
    }
  }
}

test_aria_file <- function(x) {
  if (x == "rfept") {
    test_file_path <- system.file(
      "extdata/test_datasets/qacademico/fake_data_qacademico_2020_1.csv",
      package = "sistec"
    )
  } else {
    test_file_path <- system.file(
      "extdata/test_datasets/sistec/fake_data_sistec_2019.1_2020.1.csv",
      package = "sistec"
    )
  }

  test_file <- c()
  test_file$datapath <- file.path(test_file_path)
  test_file$name <- test_file_path
  test_file
}

test_aria_server <- function(logs) {
  aria_server <- aria_server(logs = FALSE)
  server <- function(id) {
    shiny::moduleServer(id, aria_server)
  }
  server
}
