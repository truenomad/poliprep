# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

testthat::test_that("hash_table ignores row order and unnamed columns", {
  df <- data.frame(
    id = c("b", "a", "c"),
    n = c(2, 1, 3),
    other = c("x", "y", "z")
  )
  reordered <- df[c(3, 1, 2), ]
  reordered$other <- "changed"

  testthat::expect_equal(
    poliprep::hash_table(df, c("id", "n")),
    poliprep::hash_table(reordered, c("id", "n"))
  )
})

testthat::test_that("hash_table changes when a hashed value changes", {
  df <- data.frame(id = c("a", "b"), n = c(1, 2))
  edited <- df
  edited$n[2] <- 3

  testthat::expect_false(
    poliprep::hash_table(df, c("id", "n")) ==
      poliprep::hash_table(edited, c("id", "n"))
  )
})

testthat::test_that("hash_table stops on a column that is not there", {
  testthat::expect_error(
    poliprep::hash_table(data.frame(id = 1), c("id", "absent")),
    "absent"
  )
})

testthat::test_that("hash_geometry ignores feature order, not geometry", {
  pts <- sf::st_sf(
    id = c("a", "b"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
  )
  moved <- sf::st_sf(
    id = c("a", "b"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 2)))
  )

  testthat::expect_equal(
    poliprep::hash_geometry(pts, "id"),
    poliprep::hash_geometry(pts[2:1, ], "id")
  )
  testthat::expect_false(
    poliprep::hash_geometry(pts, "id") == poliprep::hash_geometry(moved, "id")
  )
})

testthat::test_that("hash_files returns NA when no file is found", {
  testthat::expect_true(
    is.na(poliprep::hash_files(file.path(tempdir(), "no_such_file.txt")))
  )
})

testthat::test_that("hash_files follows contents, not where the root sits", {
  root_a <- withr::local_tempdir()
  root_b <- withr::local_tempdir()

  for (root in c(root_a, root_b)) {
    dir.create(file.path(root, "inputs"))
    writeLines("one", file.path(root, "inputs", "a.txt"))
    writeLines("two", file.path(root, "inputs", "b.txt"))
  }

  hash_a <- poliprep::hash_files(file.path(root_a, "inputs"), root = root_a)
  hash_b <- poliprep::hash_files(file.path(root_b, "inputs"), root = root_b)

  testthat::expect_equal(hash_a, hash_b)

  writeLines("edited", file.path(root_b, "inputs", "b.txt"))

  testthat::expect_false(
    hash_a ==
      poliprep::hash_files(file.path(root_b, "inputs"), root = root_b)
  )
})

testthat::test_that("hash_files reads a single file as well as a folder", {
  root <- withr::local_tempdir()
  file <- file.path(root, "template.txt")
  writeLines("one", file)

  testthat::expect_type(poliprep::hash_files(file, root = root), "character")
  testthat::expect_false(is.na(poliprep::hash_files(file, root = root)))
})
