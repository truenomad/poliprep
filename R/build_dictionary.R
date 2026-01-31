#' Build a Data Dictionary
#'
#' Create a tidy dictionary from a data.frame. Infers type per column,
#' attaches labels from var_tree.yml, and reports stats: missing %, unique
#' count, example values, and min/max for numeric/date columns.
#'
#' @param data A data.frame or tibble to profile.
#' @param language Target language for labels ("en" or "fr"). Default is "en".
#' @param n_examples Number of example values to show. Default is 3.
#' @param max_levels Max factor levels to summarize in notes. Default is 50.
#'
#' @return A tibble with columns: variable, type, label, n, n_missing,
#'   pct_missing, n_unique, example_values, min, max, notes.
#'
#' @examples
#' df <- data.frame(
#'   country_iso3 = c("NGA", "NGA", "TCD"),
#'   adm1 = c("Kano", "Lagos", "Hadjer Lamis"),
#'   total_sampled = c(60, 58, 60)
#' )
#' build_dictionary(df)
#'
#' @export
build_dictionary <- function(data,
                             language = "en",
                             n_examples = 3L,
                             max_levels = 50L) {
  if (!base::inherits(data, "data.frame")) {
    cli::cli_abort("{.arg data} must be a data.frame or tibble.")
  }

  vars <- base::names(data)

  # handle empty data frame
  if (base::length(vars) == 0) {
    return(tibble::tibble(
      variable = character(),
      type = character(),
      label = character(),
      n = integer(),
      n_missing = integer(),
      pct_missing = double(),
      n_unique = integer(),
      example_values = character(),
      min = character(),
      max = character(),
      notes = character()
    ))
  }

  label_map <- .get_label_map(language)

  # profile each column
  rows <- base::lapply(base::seq_along(vars), function(i) {
    .profile_column(data[[i]], vars[[i]], n_examples, max_levels)
  })

  dict <- tibble::as_tibble(
    base::do.call(base::rbind, base::lapply(rows, base::as.data.frame))
  )

  # add labels
  dict$label <- base::unname(label_map[dict$variable])
  dict$label[base::is.na(dict$label)] <- dict$variable[base::is.na(dict$label)]

  # reorder columns
  dict |>
    dplyr::select(
      variable, type, label, n, n_missing, pct_missing,
      n_unique, example_values, min, max, notes
    )
}

#' Get Variable Labels from var_tree.yml
#'
#' Load variable labels from the package's var_tree.yml file.
#'
#' @param language Language code ("en" or "fr"). Default is "en".
#'
#' @return Named character vector mapping variable names to labels.
#'
#' @examples
#' labels <- get_var_labels()
#' labels["country_iso3"]
#'
#' @export
get_var_labels <- function(language = "en") {
  .get_label_map(language)
}

# internal: load and flatten polio_var_tree into a named vector
# @noRd
.get_label_map <- function(language = "en") {
  # Use polio_var_tree from package data
  tree <- tryCatch(
    get("polio_var_tree", envir = asNamespace("poliprep")),
    error = function(e) NULL
  )

  if (base::is.null(tree)) {
    return(base::character(0))
  }

  label_col <- base::paste0("label_", language)
  labels <- base::character(0)

  # flatten nested structure (lqas, surveillance, environmental sections)
  for (section_name in base::names(tree)) {
    if (base::startsWith(section_name, "_")) next

    section <- tree[[section_name]]
    if (!base::is.list(section)) next

    for (var_name in base::names(section)) {
      if (base::startsWith(var_name, "_")) next
      if (base::startsWith(var_name, "label_")) next

      var_def <- section[[var_name]]
      if (base::is.list(var_def) && label_col %in% base::names(var_def)) {
        labels[var_name] <- var_def[[label_col]]
      }
    }
  }

  labels
}

# internal: profile a single column
# @noRd
.profile_column <- function(col, name, n_examples, max_levels) {
  tp <- .guess_type(col)
  n <- base::length(col)
  n_missing <- base::sum(base::is.na(col))
  pct_missing <- if (n > 0) base::round(100 * n_missing / n, 2) else 0

  n_unique <- tryCatch(
    base::length(base::unique(col[!base::is.na(col)])),
    error = function(e) NA_integer_
  )

  example_values <- tryCatch(
    .format_examples(col, n_examples),
    error = function(e) ""
  )

  minmax <- if (tp %in% c("integer", "double", "date", "datetime")) {
    tryCatch(.range_str(col), error = function(e) c("", ""))
  } else {
    c("", "")
  }

  notes <- ""
  if (base::identical(tp, "factor")) {
    n_lv <- base::length(base::levels(col))
    notes <- if (n_lv > max_levels) {
      base::paste0("levels: ", max_levels, "+")
    } else {
      base::paste0("levels: ", n_lv)
    }
  } else if (base::identical(tp, "list")) {
    subcls <- tryCatch(
      base::unique(base::vapply(col, function(z) base::class(z)[1L], "")),
      error = function(e) character(0)
    )
    if (base::length(subcls) > 3L) subcls <- c(subcls[1:3], "...")
    notes <- base::paste0("list of: ", base::paste(subcls, collapse = ", "))
  }

  base::list(
    variable = name,
    type = tp,
    n = n,
    n_missing = n_missing,
    pct_missing = pct_missing,
    n_unique = n_unique,
    example_values = example_values,
    min = minmax[1L],
    max = minmax[2L],
    notes = notes
  )
}

# internal: guess column type
# @noRd
.guess_type <- function(x) {
  cls <- base::class(x)

  if ("sfc" %in% cls || "sfg" %in% cls) return("geometry")
  if (base::inherits(x, "POSIXt")) return("datetime")
  if (base::inherits(x, "Date")) return("date")
  if (base::is.factor(x)) return("factor")
  if (base::is.logical(x)) return("logical")
  if (base::is.integer(x)) return("integer")
  if (base::is.character(x)) return("character")
  if (base::is.list(x)) return("list")

  if (base::is.double(x)) {
    finite_x <- x[base::is.finite(x)]
    if (base::length(finite_x) > 0) {
      is_int_like <- base::all(
        base::abs(finite_x - base::round(finite_x)) < .Machine$double.eps^0.5
      )
      if (is_int_like) return("integer")
    }
    return("double")
  }

  "other"
}

# internal: format example values
# @noRd
.format_examples <- function(x, n = 3L, width = 60L) {
  x <- x[!base::is.na(x)]
  if (!base::length(x)) return("")

  vals <- if (base::is.factor(x) || base::is.character(x)) {
    tab <- base::sort(base::table(x), decreasing = TRUE)
    base::names(tab)[base::seq_len(base::min(n, base::length(tab)))]
  } else {
    base::unique(utils::head(x, n))
  }

  formatted <- if (base::is.numeric(vals) &&
                   !base::inherits(vals, c("Date", "POSIXt"))) {
    base::vapply(vals, .smart_round, character(1))
  } else {
    base::as.character(vals)
  }

  out <- base::paste(utils::head(formatted, n), collapse = ", ")

  if (base::nchar(out) > width) {
    out <- base::paste0(base::substr(out, 1L, width - 3L), "...")
  }

  out
}

# internal: smart rounding for display
# @noRd
.smart_round <- function(x) {
  if (base::is.numeric(x) && !base::inherits(x, c("Date", "POSIXt"))) {
    base::ifelse(
      x == base::floor(x),
      base::as.character(base::as.integer(x)),
      base::as.character(base::round(x, 2))
    )
  } else {
    base::as.character(x)
  }
}

# internal: compute min/max as strings
# @noRd
.range_str <- function(x) {
  x2 <- x[!base::is.na(x)]
  if (!base::length(x2)) return(c("", ""))

  rng <- base::range(x2)
  c(.smart_round(rng[1L]), .smart_round(rng[2L]))
}
