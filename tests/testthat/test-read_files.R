context("read_files")

test_that("read_sistec works", {
  skip_on_cran()
  
  sistec <- sistec::read_sistec(system.file("extdata/test_datasets/sistec",
                                            package = "sistec"))
  
  check_sistec_table(sistec, expect_nrow = 11099)
  expect_true(inherits(sistec, "sistec_data_frame"))
  
})

test_that("read_qacademico works", {
  skip_on_cran()
  
  qacademico <- sistec::read_qacademico(system.file("extdata/test_datasets/qacademico",
                                                    package = "sistec"))
  
  check_rfept_table(qacademico, expect_nrow = 14366)
  expect_true(inherits(qacademico, "rfept_data_frame"))
})

