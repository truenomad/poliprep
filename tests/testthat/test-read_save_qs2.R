# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

testthat::test_that("save and read round-trip a table through qs2", {
  testthat::skip_if_not_installed("qs2")

  file_path <- file.path(withr::local_tempdir(), "cars.qs2")

  poliprep::save(mtcars, file_path)

  testthat::expect_true(file.exists(file_path))
  testthat::expect_identical(poliprep::read(file_path), mtcars)
})

testthat::test_that("save and read round-trip a list through qs2", {
  testthat::skip_if_not_installed("qs2")

  file_path <- file.path(withr::local_tempdir(), "bundle.QS2")
  bundle <- list(data = mtcars, data_dict = data.frame(variable = "mpg"))

  poliprep::save(bundle, file_path)

  testthat::expect_identical(poliprep::read(file_path), bundle)
})
