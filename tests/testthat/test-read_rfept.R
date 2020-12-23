context("read_rfept")

test_that("read_rfept works", {
  skip_on_cran()

  qacademico <- read_rfept(system.file("extdata/examples/qacademico", package = "sistec"))

  qacademico_2 <- read_rfept(system.file("extdata/examples/qacademico", package = "sistec"),
    start = "2019.2"
  )

  check_rfept_table(qacademico, expect_nrow = 200)
  expect_true(inherits(qacademico, "rfept_data_frame"))
  check_rfept_table(qacademico_2, expect_nrow = 87)

  sigaa <- read_rfept(system.file("extdata/examples/sigaa", package = "sistec"))

  check_rfept_table(sigaa, expect_nrow = 20)
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

  check_rfept_table(sigaa, expect_nrow = 20)
  expect_true(inherits(sigaa, "rfept_data_frame"))
})

test_that("correct_course_name works", {
  skip_on_cran()
  
  wrong_course_name <- c(
    "", "aaa\\aaa", 'aaa"aaa', "aaa_aaa - aaa", "aaa/aaa", "aaa:aaa", "aaa?aaa", 
    "aaa.aaa", " aaa_aaa "
  ) 
  
  expect_equal(
    sistec:::correct_course_name(wrong_course_name),
    c("SEM CURSO", rep("aaa_aaa", 8))
  )
})

test_that("read_linked_courses works", {
  skip_on_cran()

  linked_courses <- read_linked_courses(system.file("extdata/examples/linked_courses",
    package = "sistec"
  ), "csv")

  expect_equal(nrow(linked_courses), 241)
  expect_equal(
    colnames(linked_courses),
    c("INICIO", "CICLO", "CURSO_SISTEC", "CURSO_QACADEMICO", "CAMPUS")
  )
})
