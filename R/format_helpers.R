#' Format a Percentage for Display
#'
#' Writes a percentage with one decimal below ten and none from ten upwards,
#' so a small share keeps its precision and a large one stays clean.
#'
#' @param x Numeric vector on the 0 to 100 scale.
#' @param suffix Text appended to each value. Default is `"%"`.
#'
#' @return A character vector the length of `x`. Missing values stay missing.
#'
#' @examples
#' pct_fmt(c(4.26, 85.4, NA))
#'
#' @export
pct_fmt <- function(x, suffix = "%") {
  purrr::map_chr(x, function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }
    digits <- if (abs(value) < 10) 1 else 0
    paste0(
      formatC(value, format = "f", digits = digits, big.mark = ","),
      suffix
    )
  })
}

#' Write a Count with Its Noun
#'
#' Joins a count to the singular or plural form of a noun, for a string built
#' outside the pluralisation that 'cli' provides.
#'
#' @param n Numeric vector of counts.
#' @param word The singular noun.
#' @param plural The plural noun. Default adds an "s" to `word`.
#'
#' @return A character vector the length of `n`.
#'
#' @examples
#' pluralise(1, "district")
#' pluralise(1250, "case")
#' pluralise(2, "country", "countries")
#'
#' @export
pluralise <- function(n, word, plural = paste0(word, "s")) {
  paste0(big_mark(n), " ", dplyr::if_else(n == 1, word, plural))
}

#' Collapse Whitespace in Text
#'
#' Trims leading and trailing whitespace and reduces every internal run of
#' whitespace, including tabs and line breaks, to a single space.
#'
#' @param x Character vector.
#'
#' @return A character vector the length of `x`.
#'
#' @examples
#' squish_ws("  two   words\n")
#'
#' @export
squish_ws <- function(x) gsub("[[:space:]]+", " ", trimws(x))

#' Make a Lower Snake Case Slug
#'
#' Turns free text into a slug that is safe to use as a file or folder name.
#' Every run of characters other than letters and digits becomes one
#' underscore.
#'
#' @param x Character vector.
#'
#' @return A character vector the length of `x`.
#'
#' @examples
#' slugify("Round 2 (Oct) results")
#'
#' @export
slugify <- function(x) {
  x |>
    tolower() |>
    gsub(pattern = "[^a-z0-9]+", replacement = "_") |>
    gsub(pattern = "^_|_$", replacement = "")
}

#' Normalise a Place Name into a Join Key
#'
#' Upper-cases a name, drops everything that is not a letter and collapses
#' repeated letters, so two spellings of the same place are more likely to
#' match. This raises the match rate on names but does not replace a join on
#' place codes.
#'
#' @param x Character vector of names.
#'
#' @return A character vector the length of `x`.
#'
#' @examples
#' norm_name_key(c("Nahr-e-Saraj", "NAHRE SARRAJ"))
#'
#' @export
norm_name_key <- function(x) {
  x |>
    toupper() |>
    gsub(pattern = "[^A-Z]", replacement = "") |>
    gsub(pattern = "([A-Z])\\1+", replacement = "\\1")
}

#' Normalise a GUID
#'
#' Upper-cases a GUID and removes the surrounding braces, so the same
#' identifier joins across sources that write it differently.
#'
#' @param x Vector of GUIDs.
#'
#' @return A character vector the length of `x`.
#'
#' @examples
#' norm_guid("{0a1b2c3d-0000-4000-8000-00000000abcd}")
#'
#' @export
norm_guid <- function(x) toupper(gsub("[{}]", "", as.character(x)))
