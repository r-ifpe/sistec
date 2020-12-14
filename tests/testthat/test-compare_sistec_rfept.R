context("sistec compare")

test_that("compare_sistec qacademico works", {
  skip_on_cran()

  sistec_path <- system.file("extdata/test_datasets/sistec", package = "sistec")
  qacademico_path <- system.file("extdata/test_datasets/qacademico", package = "sistec")

  qacademico <- read_rfept(qacademico_path)
  sistec <- read_sistec(sistec_path)
  comparison <- compare_sistec(sistec, qacademico)
  rows <- c(
    11099, 88, 528, 0, 1, 111, 52, # sistec
    14366, 6, 3773, 0, 43, 177, 48, # rfept
    54, 9466, 799, 239
  ) # error, situation and linked courses
  check_comparison(comparison, rows)
})

test_that("compare_sistec generic rfept works", {
  skip_on_cran()

  sistec_path <- system.file("extdata/test_datasets/generic_rfept/sistec",
    package = "sistec"
  )
  rfept_path <- system.file("extdata/test_datasets/generic_rfept/rfept",
    package = "sistec"
  )

  rfept <- read_rfept(rfept_path)
  sistec <- read_sistec(sistec_path)
  comparison <- compare_sistec(sistec, rfept)
  rows <- c(
    44, 0, 0, 0, 0, 0, 0, # sistec
    45, 0, 1, 0, 0, 0, 0, # rfept
    0, 19, 25, 1
  ) # error, situation and linked courses
  check_comparison(comparison, rows)
})
