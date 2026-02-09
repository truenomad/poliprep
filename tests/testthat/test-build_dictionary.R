test_that("build_dictionary returns expected structure", {
  df <- data.frame(
    country_iso3 = c("NGA", "NGA", "TCD"),
    adm1 = c("Kano", "Lagos", "Hadjer Lamis"),
    total_sampled = c(60, 58, 60),
    start_date = as.Date(c("2024-01-15", "2024-01-16", "2024-01-17"))
  )

  result <- build_dictionary(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_named(
    result,
    c("variable", "type", "label", "n", "n_missing", "pct_missing",
      "n_unique", "example_values", "min", "max", "notes")
  )
})

test_that("build_dictionary detects correct types", {
  df <- data.frame(
    char_col = c("a", "b", "c"),
    int_col = c(1L, 2L, 3L),
    dbl_col = c(1.5, 2.5, 3.5),
    date_col = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    lgl_col = c(TRUE, FALSE, TRUE),
    fct_col = factor(c("x", "y", "z"))
  )

  result <- build_dictionary(df)

  expect_equal(result$type[result$variable == "char_col"], "character")
  expect_equal(result$type[result$variable == "int_col"], "integer")
  expect_equal(result$type[result$variable == "dbl_col"], "double")
  expect_equal(result$type[result$variable == "date_col"], "date")
  expect_equal(result$type[result$variable == "lgl_col"], "logical")
  expect_equal(result$type[result$variable == "fct_col"], "factor")
})

test_that("build_dictionary calculates missing values correctly", {
  df <- data.frame(
    no_missing = c(1, 2, 3, 4, 5),
    some_missing = c(1, NA, 3, NA, 5),
    all_missing = c(NA, NA, NA, NA, NA)
  )

  result <- build_dictionary(df)

  expect_equal(result$n_missing[result$variable == "no_missing"], 0)
  expect_equal(result$n_missing[result$variable == "some_missing"], 2)
  expect_equal(result$n_missing[result$variable == "all_missing"], 5)

  expect_equal(result$pct_missing[result$variable == "no_missing"], 0)
  expect_equal(result$pct_missing[result$variable == "some_missing"], 40)
  expect_equal(result$pct_missing[result$variable == "all_missing"], 100)
})

test_that("build_dictionary adds labels from var_tree.yml", {
  df <- data.frame(
    country_iso3 = c("NGA", "TCD"),
    adm1 = c("Kano", "Hadjer Lamis"),
    unknown_col = c(1, 2)
  )

  result <- build_dictionary(df)

  # known variables should have labels from yml
  expect_true(grepl("ISO3", result$label[result$variable == "country_iso3"]))
  expect_true(grepl("Region|Province", result$label[result$variable == "adm1"]))

  # unknown variables fall back to variable name
  expect_equal(
    result$label[result$variable == "unknown_col"],
    "unknown_col"
  )
})

test_that("build_dictionary computes min/max for numeric columns", {
  df <- data.frame(
    nums = c(10, 20, 30, 40, 50),
    dates = as.Date(c("2024-01-01", "2024-06-15", "2024-12-31", "2024-03-01", "2024-09-15"))
  )

  result <- build_dictionary(df)

  expect_equal(result$min[result$variable == "nums"], "10")
  expect_equal(result$max[result$variable == "nums"], "50")

  expect_equal(result$min[result$variable == "dates"], "2024-01-01")
  expect_equal(result$max[result$variable == "dates"], "2024-12-31")
})

test_that("build_dictionary handles empty data frame", {
  df <- data.frame()

  result <- build_dictionary(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("get_var_labels returns named character vector", {
  labels <- get_var_labels()

  expect_type(labels, "character")
  expect_true(length(labels) > 0)
  expect_true("country_iso3" %in% names(labels))
})

test_that("get_var_labels supports French labels", {
  labels_en <- get_var_labels("en")
  labels_fr <- get_var_labels("fr")

  # both should have entries
 expect_true(length(labels_en) > 0)
  expect_true(length(labels_fr) > 0)

  # french labels should be different
  expect_false(identical(labels_en["country_iso3"], labels_fr["country_iso3"]))
})

test_that("build_dictionary with language parameter", {
  df <- data.frame(country_iso3 = c("NGA", "TCD"))

  result_en <- build_dictionary(df, language = "en")
  result_fr <- build_dictionary(df, language = "fr")

  # English label is always present in both
  expect_true("label" %in% names(result_en))
  expect_true("label" %in% names(result_fr))

  # label column is always English regardless of language param
  expect_identical(result_en$label, result_fr$label)

  # French result has an additional label_fr column
  expect_true("label_fr" %in% names(result_fr))
  expect_false("label_fr" %in% names(result_en))

  # French translation should differ from English
  expect_false(identical(result_fr$label, result_fr$label_fr))
})

test_that("build_dictionary calculates unique counts", {
  df <- data.frame(
    all_unique = c("a", "b", "c", "d", "e"),
    some_dupes = c("a", "a", "b", "b", "c"),
    all_same = c("x", "x", "x", "x", "x")
  )

  result <- build_dictionary(df)

  expect_equal(result$n_unique[result$variable == "all_unique"], 5)
  expect_equal(result$n_unique[result$variable == "some_dupes"], 3)
  expect_equal(result$n_unique[result$variable == "all_same"], 1)
})

test_that("build_dictionary generates example values", {
  df <- data.frame(
    chars = c("apple", "apple", "banana", "cherry"),
    nums = c(1, 2, 3, 4)
  )

  result <- build_dictionary(df, n_examples = 2)

  # character column shows most frequent values
  expect_true(grepl("apple", result$example_values[result$variable == "chars"]))

  # numeric column shows first values
  expect_true(nchar(result$example_values[result$variable == "nums"]) > 0)
})

test_that("build_dictionary errors on non-data.frame input", {
  expect_error(
    build_dictionary("not a data frame"),
    "must be a data.frame"
  )

  expect_error(
    build_dictionary(list(a = 1, b = 2)),
    "must be a data.frame"
  )
})

test_that("build_dictionary handles factor levels in notes", {
  # create factors with same length but different number of levels
  few_levels <- factor(rep(c("a", "b", "c"), length.out = 26))
  many_levels <- factor(letters)

  df <- data.frame(
    few_levels = few_levels,
    many_levels = many_levels
  )

  result <- build_dictionary(df, max_levels = 10)

  expect_true(grepl("levels: 3", result$notes[result$variable == "few_levels"]))
  expect_true(grepl("levels: 10\\+", result$notes[result$variable == "many_levels"]))
})
