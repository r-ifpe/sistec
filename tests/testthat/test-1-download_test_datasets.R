context("download test datasets")

test_that("test_datasets exists", {
  skip_on_cran()

  test_datasets_path <- "extdata/test_datasets"
  download_test_datasets(test_datasets_path)
  
  expect_false(system.file(test_datasets_path, package = "sistec") == "")
})