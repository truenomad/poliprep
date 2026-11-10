#' Monday of an ISO Week
#'
#' Returns the date of the Monday that opens an ISO 8601 week. The date is
#' found by arithmetic from 4 January, which always falls in week 1.
#'
#' @param year ISO year, as a number.
#' @param week ISO week number.
#'
#' @return A Date vector.
#'
#' @examples
#' iso_week_monday(2025, 1)
#' iso_week_monday(2024, c(10, 11))
#'
#' @export
iso_week_monday <- function(year, week) {
  jan4 <- as.Date(paste0(year, "-01-04"))
  week1_monday <- jan4 - (as.integer(format(jan4, "%u")) - 1)
  week1_monday + (week - 1) * 7
}

#' Calendar Half-Years
#'
#' Helpers for reporting by calendar half-year. `semester_start()` gives the
#' first day of the half-year a date falls in, `semester_end()` the last day
#' of the half-year a start date opens, and `semester_label()` a short label
#' such as "Jan-Jun 2025". `last_semesters()` lists the start dates of the
#' most recent half-years.
#'
#' @param dates Vector of dates, or values that `as.Date()` can read.
#' @param starts Vector of half-year start dates, as `semester_start()`
#'   returns them.
#' @param n Number of half-years to return.
#' @param anchor Date inside the most recent half-year to return. Default is
#'   today.
#'
#' @return `semester_start()`, `semester_end()` and `last_semesters()` return
#'   Date vectors. `semester_label()` returns a character vector.
#'
#' @examples
#' semester_start("2025-08-14")
#' semester_end(semester_start("2025-08-14"))
#' semester_label(semester_start("2025-08-14"))
#' last_semesters(4, anchor = as.Date("2025-08-14"))
#'
#' @name semester
NULL

#' @rdname semester
#' @export
semester_start <- function(dates) {
  dates <- as.Date(dates)
  as.Date(paste0(
    lubridate::year(dates),
    dplyr::if_else(lubridate::month(dates) <= 6, "-01-01", "-07-01")
  ))
}

#' @rdname semester
#' @export
semester_label <- function(starts) {
  paste0(
    dplyr::if_else(lubridate::month(starts) <= 6, "Jan-Jun ", "Jul-Dec "),
    lubridate::year(starts)
  )
}

#' @rdname semester
#' @export
semester_end <- function(starts) {
  starts <- as.Date(starts)
  as.Date(paste0(
    lubridate::year(starts),
    dplyr::if_else(lubridate::month(starts) <= 6, "-06-30", "-12-31")
  ))
}

#' @rdname semester
#' @export
last_semesters <- function(n, anchor = Sys.Date()) {
  sort(seq(semester_start(anchor), by = "-6 months", length.out = n))
}

#' Write a Date Out in Full
#'
#' Formats a date as day, month name and year, with no leading zero on the
#' day.
#'
#' @param d Vector of dates, or values that `as.Date()` can read.
#'
#' @return A character vector the length of `d`.
#'
#' @examples
#' format_long_date("2025-03-05")
#'
#' @export
format_long_date <- function(d) {
  d <- as.Date(d)
  paste0(as.integer(format(d, "%d")), " ", format(d, "%B %Y"))
}

#' Write a Reporting Window
#'
#' Formats a pair of dates as a window. The month and year are printed once
#' when both ends share them, and days carry no leading zero.
#'
#' @param from First day of the window.
#' @param to Last day of the window.
#'
#' @return A single string.
#'
#' @examples
#' format_date_window("2025-03-03", "2025-03-09")
#' format_date_window("2025-03-31", "2025-04-06")
#' format_date_window("2024-12-30", "2025-01-05")
#'
#' @export
format_date_window <- function(from, to) {
  from <- as.Date(from)
  to <- as.Date(to)
  day <- function(d) as.integer(format(d, "%d"))
  same_year <- format(from, "%Y") == format(to, "%Y")
  same_month <- same_year && format(from, "%m") == format(to, "%m")
  if (same_month) {
    glue::glue("{day(from)} to {day(to)} {format(to, '%b %Y')}")
  } else if (same_year) {
    glue::glue(
      "{day(from)} {format(from, '%b')} to {day(to)} {format(to, '%b %Y')}"
    )
  } else {
    glue::glue(
      "{day(from)} {format(from, '%b %Y')} to {day(to)} {format(to, '%b %Y')}"
    )
  }
}
