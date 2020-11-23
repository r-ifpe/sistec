context("read_sistec")

test_that("read_sistec works", {
  skip_on_cran()
  
  sistec <- read_sistec(system.file("extdata/examples/sistec",
                                    package = "sistec"))
  
  check_sistec_table(sistec, expect_nrow = 200)
  expect_true(inherits(sistec, "sistec_data_frame"))
}) 

test_that("encoding and sep work", {
  skip_on_cran()
  
  # latin1 and ,
  sistec <- read_sistec(system.file("extdata/test_datasets/sistec_encoding/latin1",
                                    package = "sistec")) 
  
  expect_true(any(stringr::str_detect(sistec$S_NO_CURSO,
                                      "\xc9|\xc7|\xd5|\xca|\xda|\xc2|\xc1|\xcd")))
  
  # UTF-8 and ;
  sistec <- read_sistec(system.file("extdata/test_datasets/sistec_encoding/utf8",
                                    package = "sistec")) 

  expect_true(any(stringi::stri_enc_isutf8(sistec$S_NO_CURSO)))
}) 

test_that("co_unidade_ensino works", {
  skip_on_cran()
  
  co_unidade_ensino <- sistec:::co_unidade_ensino()
  expect_named(co_unidade_ensino, c("CO_UNIDADE_ENSINO", "S_NO_CAMPUS"))
  
  cities <- "JABOATÃO|TUBARÃO|CAMBORIÚ|BAMBUÍ"
  n_cities <- sum(grepl(cities, co_unidade_ensino$S_NO_CAMPUS))
  expect_equal(n_cities, 4)
}) 



