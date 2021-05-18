test_that("create_pnp_list works", {
  skip_on_cran()
  
  students <- read_sistec_students(
    system.file(
      "extdata/test_pnp_critics/students", package = "sistec"
    ))
  
  ciclo <- read_ciclo(
    system.file(
      "extdata/test_pnp_critics/ciclo", package = "sistec"
    ))
  
  pnp <- sistec:::create_pnp_critics_list(students, ciclo)
  
  expect_equal(length(pnp), 5)
  expect_equal(dim(pnp$sistec), c(2767, 16))
  check_ciclo_table(pnp$ciclo_with_na, 18)
  check_sistec_students_table(pnp$students_with_na, 5)
  expect_equal(dim(pnp$pnp_student_critics), c(0, 0))
  expect_equal(dim(pnp$pnp_ciclo_critics), c(0, 0))
  
  students <- read_sistec_students(
    system.file(
      "extdata/test_pnp_critics/students", package = "sistec"
    ), start = "2020.1")
  
  ciclo <- read_ciclo(
    system.file(
      "extdata/test_pnp_critics/ciclo", package = "sistec"
    ), start = "2020.1")
  
  pnp <- sistec:::create_pnp_critics_list(students, ciclo)
  
  expect_equal(length(pnp), 5)
  expect_equal(dim(pnp$sistec), c(455, 16))
  check_ciclo_table(pnp$ciclo_with_na, 0)
  check_sistec_students_table(pnp$students_with_na, 5)
  expect_equal(dim(pnp$pnp_student_critics), c(0, 0))
  expect_equal(dim(pnp$pnp_ciclo_critics), c(0, 0))
})
