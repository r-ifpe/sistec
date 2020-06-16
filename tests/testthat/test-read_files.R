context("read_files")

test_that("read_sistec works", {
  skip_on_cran()
  
  sistec <- read_sistec(system.file("extdata/examples/sistec",
                                    package = "sistec"))
  
  check_sistec_table(sistec, expect_nrow = 200)
  expect_true(inherits(sistec, "sistec_data_frame"))
})

test_that("read_rfept works", {
  skip_on_cran()
  
  qacademico <- read_rfept(system.file("extdata/examples/qacademico", package = "sistec"))
  
  qacademico_2 <- read_rfept(system.file("extdata/examples/qacademico", package = "sistec"),
                             start = "2019.2")
  
  check_rfept_table(qacademico, expect_nrow = 200)
  expect_true(inherits(qacademico, "rfept_data_frame"))
  check_rfept_table(qacademico_2, expect_nrow = 87)
  
  sigaa <- read_rfept(system.file("extdata/examples/sigaa", package = "sistec"))
  
  check_rfept_table(sigaa, expect_nrow = 200)
  expect_true(inherits(sigaa, "rfept_data_frame"))
})

test_that("read_qacademico works", {
  skip_on_cran()
  
  qacademico <- read_qacademico(system.file("extdata/examples/qacademico", package = "sistec"))
  
  check_rfept_table(qacademico, expect_nrow = 200)
  expect_true(inherits(qacademico, "rfept_data_frame"))
})

test_that("read_sigaa works", {
  skip_on_cran()
  
  sigaa <- read_sigaa(system.file("extdata/examples/sigaa", package = "sistec"))
  
  check_rfept_table(sigaa, expect_nrow = 200)
  expect_true(inherits(sigaa, "rfept_data_frame"))
})
