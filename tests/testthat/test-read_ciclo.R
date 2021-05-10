test_that("read_ciclo works", {
  skip_on_cran()
  
  ciclo <- read_ciclo(
    system.file(
      "extdata/test_pnp_critics/ciclo", package = "sistec"
    ))
  
  check_ciclo_table(ciclo, 160)

  ciclo <- read_ciclo(
    system.file(
      "extdata/test_pnp_critics/ciclo", package = "sistec"
    ), start = "2020.1")
  
  
  check_ciclo_table(ciclo, 19)
})
