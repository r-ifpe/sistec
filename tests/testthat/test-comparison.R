context("sistec compare")

test_that("compare_sistec works", {
  skip_on_cran()
  
  sistec_path <- system.file("extdata/test_datasets/sistec", package = "sistec")
  qacademico_path <- system.file("extdata/test_datasets/qacademico", package = "sistec")
  
  comparison <- compare_sistec(sistec_path, qacademico_path)
  
  expect_equal(names(comparison),
               c("sistec_complete", "sistec_without_cpf", "sistec_without_rfept",
                 "rfept_complete", "rfept_without_cpf", "rfept_without_sistec", 
                 "situation_to_update" ))
  
  check_sistec_table(comparison$sistec_complete, expect_nrow = 11099)
  check_sistec_table(comparison$sistec_without_cpf, expect_nrow = 88) 
  check_sistec_table(comparison$sistec_without_rfept, expect_nrow = 689)
  check_rfept_table(comparison$rfept_complete, expect_nrow = 14366)
  check_rfept_table(comparison$rfept_without_cpf, expect_nrow = 6)
  check_rfept_table(comparison$rfept_without_sistec, expect_nrow = 3974)
})