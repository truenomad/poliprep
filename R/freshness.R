#' Keep a Log of When Each Data Source Was Last Refreshed
#'
#' An append-only record of data pulls, kept as a tab-separated file, and the
#' functions that read it back. `stamp_source()` adds one row for a pull.
#' `read_stamps()` returns every row. `data_as_of()` returns the latest
#' successful row for each source with its age in days. `check_freshness()`
#' prints that summary and warns about any source older than its tolerance.
#'
#' A source with no row in the log has no known refresh date and is left out
#' of the summary.
#'
#' @param source Name of the data source, such as "polis".
#' @param path Path to the log file. `stamp_source()` creates the file and
#'   its folder when they do not exist.
#' @param mode Free text describing the kind of pull. Default is "refresh".
#' @param status Outcome of the pull. Only rows with status "ok" count as a
#'   successful refresh.
#' @param n_rows Number of rows pulled.
#' @param data_min,data_max Earliest and latest date in the data pulled.
#' @param note Free text note.
#' @param tolerance_days Named vector giving, for each source, the age in
#'   days beyond which it counts as stale. A source that is not named is
#'   never reported as stale.
#' @param abort_on_stale Should a stale source stop with an error rather than
#'   a warning? Default is `FALSE`.
#' @param hint Optional line added to the error raised by `abort_on_stale`,
#'   such as where to rerun the pull.
#'
#' @return `stamp_source()` returns the row it wrote, invisibly.
#'   `read_stamps()` returns a tibble with one row per pull. `data_as_of()`
#'   returns one row per source with the added columns `age_days`,
#'   `tolerance` and `stale`. `check_freshness()` returns that table
#'   invisibly.
#'
#' @examplesIf requireNamespace("readr", quietly = TRUE)
#' log_file <- file.path(tempfile(), "data_freshness.tsv")
#'
#' stamp_source("polis", log_file, n_rows = 1200)
#' stamp_source("survey", log_file, status = "failed", note = "timeout")
#'
#' read_stamps(log_file)
#' data_as_of(log_file, tolerance_days = c(polis = 10))
#' check_freshness(log_file, tolerance_days = c(polis = 10))
#'
#' @name freshness
NULL

# column order of the log file
.freshness_cols <- c(
  "source", "run_at", "mode", "status", "n_rows", "data_min", "data_max",
  "note"
)

#' @rdname freshness
#' @export
stamp_source <- function(source, path, mode = "refresh", status = "ok",
                         n_rows = NA_integer_, data_min = NA,
                         data_max = NA, note = "") {
  rlang::check_installed("readr", reason = "to write the freshness log.")

  fs::dir_create(fs::path_dir(path))

  row <- tibble::tibble(
    source = source,
    run_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    mode = mode,
    status = status,
    n_rows = as.integer(n_rows),
    data_min = as.character(data_min),
    data_max = as.character(data_max),
    note = note
  )

  readr::write_tsv(
    row, path,
    append = fs::file_exists(path),
    col_names = !fs::file_exists(path)
  )

  cli::cli_alert_success(
    "Stamped {.val {source}}: {status}, {big_mark(row$n_rows)} rows"
  )

  invisible(row)
}

#' @rdname freshness
#' @export
read_stamps <- function(path) {
  if (!fs::file_exists(path)) {
    return(tibble::tibble(!!!stats::setNames(
      rep(list(character(0)), length(.freshness_cols)), .freshness_cols
    )))
  }

  rlang::check_installed("readr", reason = "to read the freshness log.")

  readr::read_tsv(path, show_col_types = FALSE)
}

#' @rdname freshness
#' @export
data_as_of <- function(path, tolerance_days = NULL) {
  stamps <- read_stamps(path)

  if (nrow(stamps) == 0) {
    return(stamps)
  }

  if (is.null(tolerance_days)) {
    tolerance_days <- stats::setNames(integer(0), character(0))
  }

  stamps |>
    dplyr::filter(.data$status == "ok") |>
    dplyr::group_by(.data$source) |>
    dplyr::slice_max(.data$run_at, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      age_days = as.integer(Sys.Date() - as.Date(.data$run_at)),
      tolerance = as.integer(tolerance_days[.data$source]),
      stale = !is.na(.data$tolerance) & .data$age_days > .data$tolerance
    ) |>
    dplyr::arrange(dplyr::desc(.data$stale), .data$source)
}

#' @rdname freshness
#' @export
check_freshness <- function(path, tolerance_days = NULL,
                            abort_on_stale = FALSE, hint = NULL) {
  status <- data_as_of(path, tolerance_days)

  if (nrow(status) == 0) {
    cli::cli_alert_warning("No ingest has been stamped yet")
    return(invisible(status))
  }

  print(as.data.frame(
    status[, c("source", "run_at", "age_days", "tolerance", "stale")]
  ))

  stale <- status$source[status$stale]

  if (length(stale) > 0) {
    msg <- "Past staleness tolerance: {.val {stale}}"
    if (abort_on_stale) {
      cli::cli_abort(c(msg, "i" = hint))
    }
    cli::cli_alert_warning(msg)
  } else {
    cli::cli_alert_success("Every source is within tolerance")
  }

  invisible(status)
}
