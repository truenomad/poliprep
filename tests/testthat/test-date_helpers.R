# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

testthat::test_that("iso_week_monday returns the Monday of the ISO week", {
  testthat::expect_equal(
    poliprep::iso_week_monday(2025, 1),
    as.Date("2024-12-30")
  )
  testthat::expect_equal(
    poliprep::iso_week_monday(2024, 1),
    as.Date("2024-01-01")
  )
  testthat::expect_equal(
    poliprep::iso_week_monday(2020, 53),
    as.Date("2020-12-28")
  )
})

testthat::test_that("iso_week_monday agrees with the ISO week of its date", {
  weeks <- expand.grid(year = 2015:2026, week = c(1, 2, 26, 52))
  mondays <- poliprep::iso_week_monday(weeks$year, weeks$week)

  testthat::expect_equal(format(mondays, "%u"), rep("1", nrow(weeks)))
  testthat::expect_equal(as.integer(format(mondays, "%G")), weeks$year)
  testthat::expect_equal(as.integer(format(mondays, "%V")), weeks$week)
})

testthat::test_that("semester helpers split the year at the end of June", {
  starts <- poliprep::semester_start(c("2025-06-30", "2025-07-01"))

  testthat::expect_equal(starts, as.Date(c("2025-01-01", "2025-07-01")))
  testthat::expect_equal(
    poliprep::semester_end(starts),
    as.Date(c("2025-06-30", "2025-12-31"))
  )
  testthat::expect_equal(
    poliprep::semester_label(starts),
    c("Jan-Jun 2025", "Jul-Dec 2025")
  )
})

testthat::test_that("last_semesters ends with the half-year of the anchor", {
  testthat::expect_equal(
    poliprep::last_semesters(4, anchor = as.Date("2025-08-14")),
    as.Date(c("2024-01-01", "2024-07-01", "2025-01-01", "2025-07-01"))
  )
  testthat::expect_length(poliprep::last_semesters(3), 3)
})

testthat::test_that("format_long_date drops the leading zero on the day", {
  withr::local_locale(c(LC_TIME = "C"))

  testthat::expect_equal(
    poliprep::format_long_date(c("2025-03-05", "2025-11-21")),
    c("5 March 2025", "21 November 2025")
  )
})

testthat::test_that("format_date_window prints shared month and year once", {
  withr::local_locale(c(LC_TIME = "C"))

  testthat::expect_equal(
    as.character(poliprep::format_date_window("2025-03-03", "2025-03-09")),
    "3 to 9 Mar 2025"
  )
  testthat::expect_equal(
    as.character(poliprep::format_date_window("2025-03-31", "2025-04-06")),
    "31 Mar to 6 Apr 2025"
  )
  testthat::expect_equal(
    as.character(poliprep::format_date_window("2024-12-30", "2025-01-05")),
    "30 Dec 2024 to 5 Jan 2025"
  )
})
