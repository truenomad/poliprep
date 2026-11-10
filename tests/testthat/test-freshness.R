# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

testthat::test_that("read_stamps returns an empty log when no file exists", {
  stamps <- poliprep::read_stamps(file.path(tempdir(), "absent_log.tsv"))

  testthat::expect_equal(nrow(stamps), 0)
  testthat::expect_named(
    stamps,
    c(
      "source", "run_at", "mode", "status", "n_rows", "data_min", "data_max",
      "note"
    )
  )
})

testthat::test_that("stamp_source creates the log and appends to it", {
  testthat::skip_if_not_installed("readr")

  log_file <- file.path(withr::local_tempdir(), "logs", "freshness.tsv")

  suppressMessages({
    first <- poliprep::stamp_source("polis", log_file, n_rows = 1200)
    poliprep::stamp_source(
      "survey", log_file, status = "failed", note = "timeout"
    )
  })

  stamps <- poliprep::read_stamps(log_file)

  testthat::expect_equal(first$n_rows, 1200L)
  testthat::expect_equal(nrow(stamps), 2)
  testthat::expect_equal(stamps$source, c("polis", "survey"))
  testthat::expect_equal(stamps$status, c("ok", "failed"))
  testthat::expect_equal(length(readLines(log_file)), 3)
})

testthat::test_that("data_as_of keeps the latest successful pull per source", {
  testthat::skip_if_not_installed("readr")

  log_file <- file.path(withr::local_tempdir(), "freshness.tsv")
  today <- format(Sys.Date(), "%Y-%m-%d 08:00:00")

  readr::write_tsv(
    data.frame(
      source = c("polis", "polis", "polis", "survey"),
      run_at = c("2000-01-01 08:00:00", "2000-02-01 08:00:00", today, today),
      mode = "refresh",
      status = c("ok", "ok", "failed", "ok"),
      n_rows = c(10L, 20L, NA, 5L),
      data_min = NA,
      data_max = NA,
      note = ""
    ),
    log_file
  )

  status <- poliprep::data_as_of(log_file, tolerance_days = c(polis = 10))
  polis <- status[status$source == "polis", ]
  survey <- status[status$source == "survey", ]

  testthat::expect_equal(nrow(status), 2)
  testthat::expect_equal(polis$n_rows, 20)
  testthat::expect_true(polis$stale)
  testthat::expect_equal(polis$tolerance, 10L)
  testthat::expect_equal(survey$age_days, 0L)
  testthat::expect_true(is.na(survey$tolerance))
  testthat::expect_false(survey$stale)
  testthat::expect_equal(status$source[[1]], "polis")
})

testthat::test_that("data_as_of reports nothing stale without tolerances", {
  testthat::skip_if_not_installed("readr")

  log_file <- file.path(withr::local_tempdir(), "freshness.tsv")

  suppressMessages(poliprep::stamp_source("polis", log_file))

  status <- poliprep::data_as_of(log_file)

  testthat::expect_equal(nrow(status), 1)
  testthat::expect_false(status$stale)
})

testthat::test_that("check_freshness warns or stops on a stale source", {
  testthat::skip_if_not_installed("readr")

  log_file <- file.path(withr::local_tempdir(), "freshness.tsv")

  readr::write_tsv(
    data.frame(
      source = "polis", run_at = "2000-01-01 08:00:00", mode = "refresh",
      status = "ok", n_rows = 10L, data_min = NA, data_max = NA, note = ""
    ),
    log_file
  )

  testthat::expect_message(
    testthat::expect_output(
      poliprep::check_freshness(log_file, tolerance_days = c(polis = 10))
    ),
    "Past staleness tolerance"
  )

  testthat::expect_error(
    testthat::expect_output(
      poliprep::check_freshness(
        log_file,
        tolerance_days = c(polis = 10), abort_on_stale = TRUE,
        hint = "Rerun the pull"
      )
    ),
    "Rerun the pull"
  )
})

testthat::test_that("check_freshness reports an empty log without failing", {
  testthat::expect_message(
    status <- poliprep::check_freshness(
      file.path(tempdir(), "absent_log.tsv")
    ),
    "No ingest has been stamped yet"
  )
  testthat::expect_equal(nrow(status), 0)
})
