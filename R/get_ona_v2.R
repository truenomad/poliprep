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
