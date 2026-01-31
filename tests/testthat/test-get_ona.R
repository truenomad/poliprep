testthat::test_that("get_ona_page Successfully API call processes correctly", {
  testthat::skip_on_cran()

  result <- tryCatch({
    suppressMessages(
      get_ona_page(
        api_url = "https://fakerapi.it/api/v1/addresses?_quantity=10",
        api_token = NULL
      )
    )
  }, error = function(e) {
    testthat::skip(paste("External API unavailable:", e$message))
  })

  testthat::expect_type(result$data, "list")
  testthat::expect_equal(result$status, "OK")
  testthat::expect_equal(result$code, 200)
  testthat::expect_equal(result$total, 10)
})

testthat::test_that("check_status_api when its success", {
  testthat::skip_on_cran()

  # get response
  response <- tryCatch({
    httr::HEAD("https://fakerapi.it/api/v1/addresses?_quantity=10")
  }, error = function(e) {
    testthat::skip(paste("External API unavailable:", e$message))
  })

  # Skip if API returned an error status

  if (response$status_code >= 500) {
    testthat::skip(paste("External API returned error:", response$status_code))
  }

  # test response
  testthat::expect_invisible(check_status_api(response$status_code))
})

testthat::test_that("check_status_api when it fails", {
  testthat::skip_on_cran()

  # get response
  response <- tryCatch({
    httr::HEAD("https://fakerapi.it/api/add")
  }, error = function(e) {
    testthat::skip(paste("External API unavailable:", e$message))
  })

  # test response
  testthat::expect_error(check_status_api(response))
})
