# Tests for polio_project_setup.R

# create_polio_data_structure() tests ----

testthat::test_that("create_polio_data_structure creates expected folder hierarchy", {

  temp_dir <- withr::local_tempdir()

  create_polio_data_structure(temp_dir)

  # Check main data folder exists

  testthat::expect_true(fs::dir_exists(fs::path(temp_dir, "01_data")))

  # Check foundational folders
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.1_foundational", "1.1a_admin_boundaries", "raw")
  ))

  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.1_foundational", "1.1a_admin_boundaries", "processed")
  ))
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.1_foundational", "1.1e_cache_files", "raw")
  ))

  # Check surveillance folders
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.2_surveillance", "1.2a_es", "raw")
  ))
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.2_surveillance", "1.2b_afp", "processed")
  ))

  # Check outbreak response has raw/processed directly

  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.3_outbreak_response", "raw")
  ))
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.3_outbreak_response", "processed")
  ))

  # Check 1.6_final exists but has no subfolders

  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.6_final")
  ))
  testthat::expect_false(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.6_final", "raw")
  ))
})

testthat::test_that("create_polio_data_structure creates population subfolders", {
  temp_dir <- withr::local_tempdir()

  create_polio_data_structure(temp_dir)

  # Check nested population folders

  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.1_foundational", "1.1d_population",
             "1.1ei_national", "raw")
  ))

  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "01_data", "1.1_foundational", "1.1d_population",
             "1.1eii_worldpop_rasters", "processed")
  ))
})

testthat::test_that("create_polio_data_structure returns NULL invisibly", {
  temp_dir <- withr::local_tempdir()

  result <- create_polio_data_structure(temp_dir)

  testthat::expect_null(result)
})

testthat::test_that("create_polio_data_structure handles existing directories", {
  temp_dir <- withr::local_tempdir()

  # Create structure twice - should not error
  create_polio_data_structure(temp_dir)
  testthat::expect_no_error(create_polio_data_structure(temp_dir))
})


# initialize_polio_project() tests ----

testthat::test_that("initialize_polio_project creates full project structure", {
  temp_dir <- withr::local_tempdir()

  initialize_polio_project(temp_dir)

  # Check data folder exists (created by create_polio_data_structure)
  testthat::expect_true(fs::dir_exists(fs::path(temp_dir, "01_data")))

 # Check scripts folder
  testthat::expect_true(fs::dir_exists(fs::path(temp_dir, "02_scripts")))

  # Check outputs structure
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "03_outputs", "3.1_validation", "figures")
  ))
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "03_outputs", "3.1_validation", "tables")
  ))
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "03_outputs", "3.2_intermediate_products", "figures")
  ))
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "03_outputs", "3.2_intermediate_products", "tables")
  ))
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "03_outputs", "3.3_final_outputs", "figures")
  ))
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "03_outputs", "3.3_final_outputs", "tables")
  ))
  testthat::expect_true(fs::dir_exists(
    fs::path(temp_dir, "03_outputs", "3.3_final_outputs", "powerpoint")
  ))
})

testthat::test_that("initialize_polio_project creates base_path if it doesn't exist", {
  temp_dir <- withr::local_tempdir()
  new_project <- fs::path(temp_dir, "new_project")

  testthat::expect_false(fs::dir_exists(new_project))

  initialize_polio_project(new_project)

  testthat::expect_true(fs::dir_exists(new_project))
  testthat::expect_true(fs::dir_exists(fs::path(new_project, "02_scripts")))
})

testthat::test_that("initialize_polio_project returns NULL invisibly", {
  temp_dir <- withr::local_tempdir()

  result <- initialize_polio_project(temp_dir)

  testthat::expect_null(result)
})


# setup_polio_project_paths() tests ----

testthat::test_that("setup_polio_project_paths returns named list of paths", {
  temp_dir <- withr::local_tempdir()
  initialize_polio_project(temp_dir)

  paths <- setup_polio_project_paths(base_path = temp_dir, quiet = TRUE)

  testthat::expect_type(paths, "list")
  testthat::expect_true(length(paths) > 0)

  # Check key path names exist
  expected_names <- c(
    "core", "admin_shp", "physical_feat", "settlements",
    "pop_national", "pop_worldpop", "cache",
    "es", "afp", "esurv",
    "outbreak", "orq_pre", "orq_intra", "orq_post",
    "dhs", "final_data",
    "val", "interm", "final",
    "val_fig", "val_tbl",
    "interm_fig", "interm_tbl",
    "final_fig", "final_tbl", "final_ppt"
  )

  for (name in expected_names) {
    testthat::expect_true(name %in% names(paths),
      info = paste("Missing path:", name))
  }
})

testthat::test_that("setup_polio_project_paths sets global option", {
  temp_dir <- withr::local_tempdir()
  initialize_polio_project(temp_dir)

  # Clear option first
 options(polio.paths = NULL)

  paths <- setup_polio_project_paths(base_path = temp_dir, quiet = TRUE)

  opt_paths <- getOption("polio.paths")
  testthat::expect_false(is.null(opt_paths))
  testthat::expect_equal(paths, opt_paths)
})

testthat::test_that("setup_polio_project_paths warns about missing folders", {
  temp_dir <- withr::local_tempdir()
  # Don't initialize - folders won't exist

  testthat::expect_message(
    setup_polio_project_paths(base_path = temp_dir, quiet = FALSE),
    "Missing"
  )
})

testthat::test_that("setup_polio_project_paths quiet suppresses warnings", {
  temp_dir <- withr::local_tempdir()
  # Don't initialize - folders won't exist

  testthat::expect_no_message(
    setup_polio_project_paths(base_path = temp_dir, quiet = TRUE)
  )
})

testthat::test_that("setup_polio_project_paths paths point to correct locations", {
  temp_dir <- withr::local_tempdir()
  initialize_polio_project(temp_dir)

  paths <- setup_polio_project_paths(base_path = temp_dir, quiet = TRUE)

  # Check specific path mappings
 testthat::expect_true(grepl("1.1a_admin_boundaries$", paths$admin_shp))
  testthat::expect_true(grepl("1.2a_es$", paths$es))
  testthat::expect_true(grepl("1.2b_afp$", paths$afp))
  testthat::expect_true(grepl("3.3_final_outputs/powerpoint$", paths$final_ppt))
})

testthat::test_that("setup_polio_project_paths uses current dir as fallback", {
  temp_dir <- withr::local_tempdir()

  withr::with_dir(temp_dir, {
    paths <- setup_polio_project_paths(base_path = NULL, quiet = TRUE)
    testthat::expect_equal(
      normalizePath(paths$core, winslash = "/"),
      normalizePath(temp_dir, winslash = "/")
    )
  })
})
