context("sistec compare")

test_that("compare_sistec works", {
  skip_on_cran()
  
  sistec_path <- system.file("extdata/test_datasets/sistec", package = "sistec")
  qacademico_path <- system.file("extdata/test_datasets/qacademico", package = "sistec")
  
  qacademico <- read_rfept(qacademico_path)
  sistec <- read_sistec(sistec_path)
  
  comparison <- compare_sistec(sistec, qacademico)
  
  expect_equal(names(comparison),
               c("sistec_complete", "sistec_without_cpf", "sistec_without_rfept",
                 "rfept_complete", "rfept_without_cpf", "rfept_without_sistec",
                 "rfept_wrong_beginning", "rfept_wrong_cyclo",
                 "situation_updated", "situation_to_update", "linked_courses"))
  
  check_sistec_table(comparison$sistec_complete, expect_nrow = 11099)
  check_sistec_table(comparison$sistec_without_cpf, expect_nrow = 88) 
  check_sistec_table(comparison$sistec_without_rfept, expect_nrow = 648)
  check_rfept_table(comparison$rfept_complete, expect_nrow = 14366)
  check_rfept_table(comparison$rfept_without_cpf, expect_nrow = 6)
  check_rfept_table(comparison$rfept_without_sistec, expect_nrow = 3975)
  check_rfept_wrong_registration(comparison$rfept_wrong_beginning, expect_nrow = 56)
  check_rfept_wrong_registration(comparison$rfept_wrong_cyclo, expect_nrow = 0)
  check_situation_table(comparison$situation_updated, expect_nrow = 9505)
  check_situation_table(comparison$situation_to_update, expect_nrow = 817)
  check_linked_courses_table(comparison$linked_courses, expect_nrow = 241)
})
