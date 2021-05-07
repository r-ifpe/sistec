test_that("read_ciclo works", {
  skip_on_cran()
  
  ciclo <- read_ciclo(
    system.file(
      "extdata/test_datasets/pnp_critics/ciclo", package = "sistec"
    ))
  
  expect_equal(nrow(ciclo), 160)
  expect_equal(
    names(ciclo),
    c(
      "C_CO_CICLO_MATRICULA", "C_NO_CAMPUS", "C_NO_SUBTIPO", "C_NU_CARGA_HORARIA",
      "C_DT_FIM", "C_DT_INICIO", "C_DT_CRIACAO", "C_NU_QTD_MATRICULAS", "C_NU_QTD_VAGAS",
      "C_NU_QTD_INSCRITOS", "C_DT_INICIO_ANO_SEMESTRE"
  ))
  
  ciclo <- read_ciclo(
    system.file(
      "extdata/test_datasets/pnp_critics/ciclo", package = "sistec"
    ), start = "2020.1")
  
  
  expect_equal(nrow(ciclo), 19)
  expect_equal(
    names(ciclo),
    c(
      "C_CO_CICLO_MATRICULA", "C_NO_CAMPUS", "C_NO_SUBTIPO", "C_NU_CARGA_HORARIA",
      "C_DT_FIM", "C_DT_INICIO", "C_DT_CRIACAO", "C_NU_QTD_MATRICULAS", "C_NU_QTD_VAGAS",
      "C_NU_QTD_INSCRITOS", "C_DT_INICIO_ANO_SEMESTRE"
    ))
})
