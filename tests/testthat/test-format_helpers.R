# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

testthat::test_that("pct_fmt keeps one decimal below ten and none above", {
  testthat::expect_equal(
    poliprep::pct_fmt(c(4.26, 85.4, 10, 1234.4, -3.14)),
    c("4.3%", "85%", "10%", "1,234%", "-3.1%")
  )
})

testthat::test_that("pct_fmt keeps missing values and empty input", {
  testthat::expect_equal(
    poliprep::pct_fmt(c(NA, 50)),
    c(NA_character_, "50%")
  )
  testthat::expect_equal(poliprep::pct_fmt(numeric(0)), character(0))
})

testthat::test_that("pct_fmt uses the suffix it is given", {
  testthat::expect_equal(poliprep::pct_fmt(50, suffix = ""), "50")
  testthat::expect_equal(
    poliprep::pct_fmt(2.5, suffix = " percent"),
    "2.5 percent"
  )
})

testthat::test_that("pluralise picks the singular only for one", {
  testthat::expect_equal(poliprep::pluralise(1, "district"), "1 district")
  testthat::expect_equal(poliprep::pluralise(0, "district"), "0 districts")
  testthat::expect_equal(poliprep::pluralise(1250, "case"), "1,250 cases")
  testthat::expect_equal(
    poliprep::pluralise(c(1, 2), "case"),
    c("1 case", "2 cases")
  )
  testthat::expect_equal(
    poliprep::pluralise(2, "country", "countries"),
    "2 countries"
  )
})

testthat::test_that("squish_ws collapses every kind of whitespace", {
  testthat::expect_equal(
    poliprep::squish_ws(c("  two   words\n", "tab\tand\n\nbreaks", "")),
    c("two words", "tab and breaks", "")
  )
})

testthat::test_that("slugify returns lower snake case", {
  testthat::expect_equal(
    poliprep::slugify(c("Round 2 (Oct) results", "--A--", "already_fine")),
    c("round_2_oct_results", "a", "already_fine")
  )
})

testthat::test_that("norm_name_key matches spellings of the same place", {
  keys <- poliprep::norm_name_key(c("Nahr-e-Saraj", "NAHRE SARRAJ"))

  testthat::expect_equal(keys[[1]], "NAHRESARAJ")
  testthat::expect_equal(keys[[1]], keys[[2]])
})

testthat::test_that("norm_guid upper-cases and drops braces", {
  testthat::expect_equal(
    poliprep::norm_guid(c(
      "{0a1b2c3d-0000-4000-8000-00000000abcd}",
      "0A1B2C3D-0000-4000-8000-00000000ABCD"
    )),
    rep("0A1B2C3D-0000-4000-8000-00000000ABCD", 2)
  )
})
