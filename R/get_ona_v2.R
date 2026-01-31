#' Download ONA Form Data
#'
#' Fetches data from the ONA API (whonghub) with automatic pagination,
#' flattens nested JSON structures, and optionally saves to disk.
#'
#' @section Authentication:
#' Requires an ONA API token. Set via the `ONA_API_TOKEN` environment variable
#' or pass directly to the `token` parameter. Environment variable takes
#' precedence if `token` is NULL.
#'
#' @section GPS Parsing:
#' If columns named `GPS_hh` or `GPS_hh_end` exist, they are automatically
#' split into latitude, longitude, altitude, and precision components with
#' prefixed column names (e.g., `_GPS_hh_latitude`).
#'
#' @param form_id Integer. The ONA form ID to download.
#' @param token Character. ONA API token. If NULL, reads from `ONA_API_TOKEN`
#'   environment variable.
#' @param base_url Character. API base URL. Default is whonghub API.
#' @param page_size Integer. Number of records per page. Default 10000.
#' @param output_path Character or NULL. If provided, saves the result as an
#'   RDS file (or QS2 if `use_qs = TRUE`) to this path.
#' @param use_qs Logical. Use `qs2` package for faster serialization. Default
#'   FALSE (uses base `saveRDS`).
#' @param quiet Logical. Suppress progress messages. Default FALSE.
#'
#' @return A tibble containing the flattened form data. Returns NULL invisibly
#'   if no data is available.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # set token in environment
#' Sys.setenv(ONA_API_TOKEN = "your_token_here")
#'
#' # download a single form
#' data <- download_ona_form(form_id = 8588)
#'
#' # download and save to file
#' download_ona_form(
#'   form_id = 8588,
#'   output_path = "data/form_8588.rds"
#' )
#' }
download_ona_form <- function(form_id,
                              token = NULL,
                              base_url = "https://api.whonghub.org/api/v1/data",
                              page_size = 10000L,
                              output_path = NULL,
                              use_qs = FALSE,
                              quiet = FALSE) {
  # input validation
 if (!is.numeric(form_id) || length(form_id) != 1) {
    cli::cli_abort("{.arg form_id} must be a single integer.")
  }

  # resolve token
 token <- token %||% Sys.getenv("ONA_API_TOKEN", unset = "")
  if (token == "") {
    cli::cli_abort(
      c("No API token provided.",
        "i" = "Set {.envvar ONA_API_TOKEN} or pass {.arg token} directly.")
    )
  }

  # fetch all pages
  all_data <- .fetch_ona_pages(
    form_id = form_id,
    token = token,
    base_url = base_url,
    page_size = page_size,
    quiet = quiet
  )

  if (length(all_data) == 0) {
    if (!quiet) cli::cli_alert_warning("No data available for form {form_id}.")
    return(invisible(NULL))
  }

  # flatten and convert to tibble
  flat_list <- lapply(all_data, .flatten_nested)
  result <- dplyr::bind_rows(flat_list)

  # parse GPS columns if present
 result <- .parse_gps_columns(result)

  if (!quiet) {
    cli::cli_alert_success(
      "Downloaded {nrow(result)} records with {ncol(result)} columns."
    )
  }

  # save if output path provided
  if (!is.null(output_path)) {
    dir_path <- fs::path_dir(output_path)
    if (!fs::dir_exists(dir_path)) {
      fs::dir_create(dir_path, recurse = TRUE)
    }

    if (use_qs) {
      if (!requireNamespace("qs2", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg qs2} required when {.arg use_qs = TRUE}.")
      }
      qs2::qs_save(result, output_path)
    } else {
      saveRDS(result, output_path)
    }

    if (!quiet) cli::cli_alert_success("Saved to {.path {output_path}}.")
 }

  result
}

#' Download Multiple ONA Forms
#'
#' Downloads multiple ONA forms and optionally routes them to different
#' output directories based on form content (e.g., IM vs LQAS).
#'
#' @param form_ids Integer vector. ONA form IDs to download.
#' @param token Character. ONA API token.
#' @param output_dir_im Character or NULL. Directory for IM data (forms with
#'   "Type_Monitoring" column).
#' @param output_dir_lqas Character or NULL. Directory for LQAS data (forms
#'   without "Type_Monitoring" column).
#' @param use_qs Logical. Use `qs2` package for serialization. Default FALSE.
#' @param quiet Logical. Suppress progress messages. Default FALSE.
#'
#' @return A named list of tibbles, keyed by form ID. Forms with no data
#'   return NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- download_ona_forms(
#'   form_ids = c(8588, 8589),
#'   output_dir_im = "data/raw/im",
#'   output_dir_lqas = "data/raw/lqas"
#' )
#' }
download_ona_forms <- function(form_ids,
                               token = NULL,
                               output_dir_im = NULL,
                               output_dir_lqas = NULL,
                               use_qs = FALSE,
                               quiet = FALSE) {
  if (!is.numeric(form_ids) || length(form_ids) == 0) {
    cli::cli_abort("{.arg form_ids} must be a non-empty integer vector.")
  }

  # create output dirs if specified
  if (!is.null(output_dir_im)) fs::dir_create(output_dir_im, recurse = TRUE)
  if (!is.null(output_dir_lqas)) fs::dir_create(output_dir_lqas, recurse = TRUE)

  results <- list()
  ext <- if (use_qs) "qs" else "rds"

  for (fid in form_ids) {
    if (!quiet) cli::cli_h2("Form {fid}")

    data <- download_ona_form(
      form_id = fid,
      token = token,
      use_qs = use_qs,
      quiet = quiet
    )

    if (is.null(data)) {
      results[[as.character(fid)]] <- NULL
      next
    }

    # route to appropriate directory
    if (!is.null(output_dir_im) || !is.null(output_dir_lqas)) {
      is_im <- "Type_Monitoring" %in% names(data)
      out_dir <- if (is_im) output_dir_im else output_dir_lqas

      if (!is.null(out_dir)) {
        out_file <- fs::path(out_dir, paste0(fid, ".", ext))

        if (use_qs) {
          qs2::qs_save(data, out_file)
        } else {
          saveRDS(data, out_file)
        }

        if (!quiet) cli::cli_alert_success("Saved to {.path {out_file}}.")
      }
    }

    results[[as.character(fid)]] <- data
  }

  results
}


# internal: fetch all pages from ONA API
.fetch_ona_pages <- function(form_id, token, base_url, page_size, quiet) {
  all_data <- list()
  page <- 1L
  has_more <- TRUE

  while (has_more) {
    url <- paste0(base_url, "/", form_id)

    response <- httr::GET(
      url,
      httr::add_headers(Authorization = paste("Token", token)),
      query = list(page = page, page_size = page_size)
    )

    status <- httr::status_code(response)

    if (status == 200) {
      data <- httr::content(response, as = "parsed", simplifyVector = FALSE)

      if (length(data) == 0) {
        has_more <- FALSE
      } else {
        all_data <- c(all_data, data)
        if (!quiet) {
          cli::cli_alert_info("Fetched page {page} ({length(data)} records).")
        }
        page <- page + 1L
      }
    } else if (status %in% c(401, 403)) {
      cli::cli_abort(
        c("Authentication failed for form {form_id}.",
          "i" = "Check your API token and form permissions.")
      )
    } else {
      cli::cli_abort("API request failed with status {status}.")
    }
  }

  all_data
}

# internal: flatten nested list to single level
.flatten_nested <- function(x, parent_key = "", sep = "/") {
  result <- list()

  for (key in names(x)) {
    value <- x[[key]]

    # build new key
    new_key <- if (parent_key == "") key else paste0(parent_key, sep, key)

    if (is.list(value) && !is.null(names(value))) {
      # named list (dict) - recurse
      result <- c(result, .flatten_nested(value, new_key, sep))
    } else if (is.list(value) && is.null(names(value))) {
      # unnamed list (array) - index each element
      for (i in seq_along(value)) {
        item <- value[[i]]
        indexed_key <- paste0(new_key, "[", i, "]")

        if (is.list(item) && !is.null(names(item))) {
          result <- c(result, .flatten_nested(item, indexed_key, sep))
        } else {
          result[[indexed_key]] <- item
        }
      }
    } else {
      # scalar value
      result[[new_key]] <- value
    }
  }

  result
}

# internal: parse GPS columns into components
.parse_gps_columns <- function(data) {
  gps_cols <- c("GPS_hh", "GPS_hh_end")

  for (col in gps_cols) {
    if (!col %in% names(data)) next

    prefix <- paste0("_", col, "_")
    components <- c("latitude", "longitude", "altitude", "precision")

    # split on whitespace
    split_vals <- stringr::str_split(data[[col]], "\\s+", simplify = TRUE)

    # assign components (handle missing values)
    for (i in seq_along(components)) {
      new_col <- paste0(prefix, components[i])
      data[[new_col]] <- if (ncol(split_vals) >= i) split_vals[, i] else NA_character_
    }
  }

  data
}
