test_that("pnp_beginning_date_gt_beginning_ciclo_date works", {
  skip_on_cran()
  
  students <- read_sistec_students(
    system.file(
      "extdata/test_pnp_critics/students", package = "sistec"
    ))
  
  ciclo <- read_ciclo(
    system.file(
      "extdata/test_pnp_critics/ciclo", package = "sistec"
    ))
  
  pnp <- create_pnp_critics_list(students, ciclo)
  pnp$sistec$C_DT_CRIACAO[1:3] <- c("2019-11-30", "2020-11-01", "2020-10-30")
  pnp <- pnp_beginning_date_gt_beginning_ciclo_date(pnp)
  
  check_students_critics_table(pnp$pnp_student_critics, 3)
})
