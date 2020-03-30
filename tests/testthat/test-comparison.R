context("sistec compare")

test_that("qacademico comparison works", {
  skip_on_cran()
  
  comparison <- compare_sistec_qacademico(sistec_path = system.file("extdata/sistec", package = "sistec"),
                        qacademico_path = system.file("extdata/qacademico", package = "sistec"))
  
  
  expect_equal(length(comparison$situation), 15) # amount of ciclos
  expect_equal(names(comparison$situation[[1]]),
               c("Nome_q", "Nome_sistec", "Cpf", "Ciclo", "Status_q", "Status_sistec"))
  
  # screen output
  total_students <- nrow(comparison$ifpe_dados)
  multi_vinculo <- nrow(comparison$situation$multi_vinculo)
  
  students_to_update <- sum(unlist(
    lapply(comparison$situation, nrow))) - multi_vinculo
  
  students_updated <- total_students - students_to_update - multi_vinculo
  
  expect_equal(total_students, 1111)
  expect_equal(multi_vinculo, 11)
  expect_equal(students_updated, 875)
})