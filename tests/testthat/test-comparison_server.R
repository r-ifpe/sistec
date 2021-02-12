context("comparison_server")

test_that("beginning buttons are blanked", {
  skip_on_cran()

  # suppress just a shiny warning
  suppressWarnings(
    shiny::testServer(comparison_server, args = list(logs = FALSE), {
      expect_null(input$rfept)
      expect_null(input$sistec)
      expect_null(output$compare_button$html)
      expect_null(output$download_button$html)
    })
  )
})

test_that("compare_button only appears after upload the files", {
  skip_on_cran()

  # start with rfept
  shiny::testServer(comparison_server, args = list(logs = FALSE), {
    expect_null(output$compare_button$html)

    rfept <- test_aria_file("rfept")
    session$setInputs(rfept = rfept)
    expect_null(output$compare_button$html)

    sistec <- test_aria_file("sistec")
    session$setInputs(sistec = sistec)
    expect_true(grepl('id="proxy1-compare_button"', output$compare_button$html))
  })

  # start with sistec
  shiny::testServer(comparison_server, args = list(logs = FALSE), {
    expect_null(output$compare_button$html)

    sistec <- test_aria_file("sistec")
    session$setInputs(sistec = sistec)
    expect_null(output$compare_button$html)

    rfept <- test_aria_file("rfept")
    session$setInputs(rfept = rfept)
    expect_true(grepl('id="proxy1-compare_button"', output$compare_button$html))
  })
})

test_that("download_button only appears after compare_button appears", {
  skip_on_cran()

  shiny::testServer(comparison_server, args = list(logs = FALSE), {
    expect_null(output$download_button$html)

    session$setInputs(compare_button = 0)
    expect_null(output$download_button$html)

    session$setInputs(compare_button = 1)
    expect_true(grepl('id="proxy1-download_online"', output$download_button$html))
  })
})
