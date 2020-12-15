context("aria_server")

test_that("beginning buttons are blanked", {
  skip_on_cran()

  server <- test_aria_server(logs = FALSE)
  
  # suppress just a shiny warning
  suppressWarnings(
    shiny::testServer(server, {
      expect_null(input$rfept)
      expect_null(input$sistec)
      expect_null(output$compare_button$html)
      expect_null(output$download_button$html)
    })
  )
})

test_that("compare_button only appears after upload the files", {
  skip_on_cran()

  server <- test_aria_server(logs = FALSE)

  # start with rfept
  shiny::testServer(server, {
    expect_null(output$compare_button$html)

    rfept <- test_aria_file("rfept")
    session$setInputs(rfept = rfept)
    expect_null(output$compare_button$html)

    sistec <- test_aria_file("sistec")
    session$setInputs(sistec = sistec)
    expect_true(grepl('id="compare_button"', output$compare_button$html))
  })

  # start with sistec
  shiny::testServer(server, {
    expect_null(output$compare_button$html)

    sistec <- test_aria_file("sistec")
    session$setInputs(sistec = sistec)
    expect_null(output$compare_button$html)

    rfept <- test_aria_file("rfept")
    session$setInputs(rfept = rfept)
    expect_true(grepl('id="compare_button"', output$compare_button$html))
  })
})

test_that("download_button only appears after compare_button appears", {
  skip_on_cran()

  server <- test_aria_server(logs = FALSE)
  shiny::testServer(server, {
    expect_null(output$download_button$html)

    session$setInputs(compare_button = 0)
    expect_null(output$download_button$html)

    session$setInputs(compare_button = 1)
    expect_true(grepl('id="download_online"', output$download_button$html))
  })
})
