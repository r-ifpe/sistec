context("sistec compare")

test_that("compare_sistec works", {
  skip_on_cran()
  
  sistec_path <- system.file("extdata/test_datasets/sistec", package = "sistec")
  qacademico_path <- system.file("extdata/test_datasets/qacademico", package = "sistec")
  linked_courses_path <- system.file("extdata/examples/linked_courses", package = "sistec")
  
  qacademico <- read_rfept(qacademico_path)
  sistec <- read_sistec(sistec_path)
  linked_courses <- read_linked_courses(linked_courses_path, "csv")
  
  # with ARIA approach
  comparison <- compare_sistec(sistec, qacademico)
  check_comparison(comparison)

  # with file approach
  comparison <- compare_sistec(sistec, qacademico, linked_courses)
  check_comparison(comparison)
})
