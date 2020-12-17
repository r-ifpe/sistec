context("read_sistec")

test_that("read_sistec works", {
  skip_on_cran()

  sistec <- read_sistec(system.file("extdata/examples/sistec",
    package = "sistec"
  ))

  check_sistec_table(sistec, expect_nrow = 200)
  expect_true(inherits(sistec, "sistec_data_frame"))
})

test_that("encoding and sep work", {
  skip_on_cran()
  
  latin1_characters <- "\xc9|\xc7|\xd5|\xca|\xda|\xc2|\xc1|\xcd" # bug in \xc3
  utf_8_characters <- "\u00c9|\u00c7|\u00d5|\u00ca|\u00da|\u00c2|\u00c1|\u00cd"
  windows <- grepl("windows", tolower(Sys.getenv("SystemRoot")))
  
  # latin1 and ,
  sistec <- read_sistec(system.file("extdata/test_datasets/sistec_encoding/latin1",
    package = "sistec"
  ))

  if (windows) {
    expect_true(any(stringr::str_detect(sistec$S_NO_CURSO, latin1_characters)))
  } else {
    expect_true(any(stringr::str_detect(sistec$S_NO_CURSO, utf_8_characters)))
  }

  # UTF-8 and ;
  sistec <- read_sistec(system.file("extdata/test_datasets/sistec_encoding/utf8",
    package = "sistec"
  ))

  expect_true(any(stringr::str_detect(sistec$S_NO_CURSO, utf_8_characters)))
})

test_that("co_unidade_ensino works", {
  skip_on_cran()

  co_unidade_ensino <- sistec:::co_unidade_ensino()
  expect_named(co_unidade_ensino, c("S_NO_CAMPUS", "CO_UNIDADE_ENSINO"))

  cities <- "JABOAT\u00c3O|CHAPEC\u00d3|CAMBORI\u00da|BAMBU\u00cd"
  n_cities <- sum(stringr::str_detect(co_unidade_ensino$S_NO_CAMPUS, cities))
  expect_equal(n_cities, 4)
})
