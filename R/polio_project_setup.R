#' Create Polio Data Folder Structure
#'
#' Creates a polio-specific folder hierarchy under `01_data/` with `raw/` and
#' `processed/` subfolders for each data domain. This is the data-only portion
#' of the project structure; use `initialize_polio_project()` for the full
#' project setup including scripts and outputs.
#'
#' @section Folder Structure:
#' The function creates the following hierarchy:
#'
#' ```
#' 01_data/
#' ├── 1.1_foundational/
#' │   ├── 1.1a_admin_boundaries/       (admin shapefiles)
#' │   ├── 1.1b_physical_features/      (rivers, roads, etc.)
#' │   ├── 1.1c_masterlist_of_settlements/
#' │   ├── 1.1d_population/
#' │   │   ├── 1.1ei_national/          (national census data)
#' │   │   └── 1.1eii_worldpop_rasters/ (WorldPop rasters)
#' │   └── 1.1e_cache_files/            (intermediate cached files)
#' ├── 1.2_surveillance/
#' │   ├── 1.2a_es/                     (environmental surveillance)
#' │   ├── 1.2b_afp/                    (acute flaccid paralysis)
#' │   └── 1.2c_esurv/                  (e-surveillance)
#' ├── 1.3_outbreak_response/           (outbreak response data)
#' ├── 1.4_outbreak_response_quality/
#' │   ├── 1.4a_pre_campaign/           (pre-campaign assessments)
#' │   ├── 1.4b_intra_campaign/         (intra-campaign monitoring)
#' │   └── 1.4c_post_campaign/          (post-campaign evaluation)
#' ├── 1.5_health_systems/
#' │   └── 1.5a_dhs/                    (DHS survey data)
#' └── 1.6_final/                       (final analysis-ready datasets)
#' ```
#'
#' Each leaf folder (except `1.6_final`) contains `raw/` and `processed/`
#' subfolders.
#'
#' @param base_path Character. Root directory for the data folders.
#'   Default is current directory.
#'
#' @return NULL invisibly (creates folders on disk)
#' @export
#'
#' @examples
#' tmp <- tempdir()
#' create_polio_data_structure(tmp)
#' fs::dir_tree(fs::path(tmp, "01_data"), recurse = 1)
create_polio_data_structure <- function(base_path = ".") {
  # folders that should NOT have raw/processed subfolders
  no_subfolders <- character(0)

  data_structure <- list(
    "1.1_foundational" = c(
      "1.1a_admin_boundaries",
      "1.1b_physical_features",
      "1.1c_masterlist_of_settlements",
      "1.1d_population/1.1ei_national",
      "1.1d_population/1.1eii_worldpop_rasters",
      "1.1e_cache_files"
    ),
    "1.2_surveillance" = c(
      "1.2a_es",
      "1.2b_afp",
      "1.2c_esurv"
    ),
    "1.3_outbreak_response" = ".",
    "1.4_outbreak_response_quality" = c(
      "1.4a_pre_campaign",
      "1.4b_intra_campaign",
      "1.4c_post_campaign"
    ),
    "1.5_health_systems" = c(
      "1.5a_dhs"
    ),
    "1.6_final" = FALSE
  )

  base_data <- fs::path(base_path, "01_data")

  for (domain in names(data_structure)) {
    domain_path <- fs::path(base_data, domain)
    entries <- data_structure[[domain]]

    if (identical(entries, FALSE)) {
      # create domain folder only, no subfolders
      fs::dir_create(domain_path)
    } else if (identical(entries, ".")) {
      # create raw/processed directly in domain
      fs::dir_create(fs::path(domain_path, "raw"))
      fs::dir_create(fs::path(domain_path, "processed"))
    } else {
      # create raw/processed in each subfolder (except cache)
      for (entry in entries) {
        path_final <- fs::path(domain_path, entry)
        if (basename(entry) %in% no_subfolders) {
          fs::dir_create(path_final)
        } else {
          fs::dir_create(fs::path(path_final, "raw"))
          fs::dir_create(fs::path(path_final, "processed"))
        }
      }
    }
  }

  invisible(NULL)
}

#' Initialize Full Polio Project Structure
#'
#' Sets up a complete polio project hierarchy with standardized folders for
#' data, scripts, and outputs. This is the main entry point for creating a new
#' polio analysis project.
#'
#' @section Project Structure:
#' The function creates:
#'
#' ```
#' project_root/
#' ├── 01_data/                         (see create_polio_data_structure())
#' ├── 02_scripts/                      (R scripts for analysis)
#' └── 03_outputs/
#'     ├── 3.1_validation/
#'     │   ├── figures/                 (validation plots)
#'     │   └── tables/                  (validation tables)
#'     ├── 3.2_intermediate_products/
#'     │   ├── figures/                 (intermediate plots)
#'     │   └── tables/                  (intermediate tables)
#'     └── 3.3_final_outputs/
#'         ├── figures/                 (final figures for reports)
#'         ├── tables/                  (final tables for reports)
#'         └── powerpoint/              (PowerPoint exports)
#' ```
#'
#' @section Workflow:
#' 1. Call `initialize_polio_project()` once to create the folder structure
#' 2. Call `setup_polio_project_paths()` at the start of each script to get
#'    standardized path references
#'
#' @param base_path Character. Project root directory. Creates the directory
#'   if it does not exist. Default is current directory (".").
#'
#' @return NULL invisibly (folders created on disk)
#' @export
#'
#' @examples
#' tmp <- tempdir()
#' initialize_polio_project(tmp)
#' fs::dir_tree(tmp, recurse = 2)
initialize_polio_project <- function(base_path = ".") {
  if (!fs::dir_exists(base_path)) {
    fs::dir_create(base_path)
  }

  # create polio data domains

  create_polio_data_structure(base_path)

  # scripts folder
  fs::dir_create(fs::path(base_path, "02_scripts"))

  # outputs structure
  fs::dir_create(fs::path(base_path, "03_outputs", "3.1_validation", "figures"))
  fs::dir_create(fs::path(base_path, "03_outputs", "3.1_validation", "tables"))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.2_intermediate_products",
    "figures"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.2_intermediate_products",
    "tables"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.3_final_outputs",
    "figures"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.3_final_outputs",
    "tables"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.3_final_outputs",
    "powerpoint"
  ))

  invisible(NULL)
}

#' Setup Polio Project Paths
#'
#' Creates a named list of standardized paths for polio pipelines. Call this
#' at the start of each analysis script to get consistent path references.
#' Detects project root automatically and constructs all data/output paths
#' with short, memorable names. Validates that folders exist and warns if not.
#'
#' @section Root Detection:
#' When `base_path` is NULL, the function tries to detect the project root:
#' 1. Uses `here::here()` if the `here` package is available
#' 2. Falls back to `rprojroot` heuristics (looks for `.here` file or git root)
#' 3. Final fallback: current working directory
#'
#' @section Path Keys:
#' The returned list contains these named paths:
#'
#' **Foundational data (1.1):**
#' \describe{
#'   \item{`core`}{Project root directory}
#'   \item{`admin_shp`}{Administrative boundary shapefiles}
#'   \item{`physical_feat`}{Physical features (rivers, roads, etc.)}
#'   \item{`settlements`}{Master list of settlements}
#'   \item{`pop_national`}{National census population data}
#'   \item{`pop_worldpop`}{WorldPop population rasters}
#'   \item{`cache`}{Cached intermediate files}
#' }
#'
#' **Surveillance data (1.2):**
#' \describe{
#'   \item{`es`}{Environmental surveillance data}
#'   \item{`afp`}{Acute flaccid paralysis case data}
#'   \item{`esurv`}{E-surveillance data}
#' }
#'
#' **Outbreak response (1.3-1.4):**
#' \describe{
#'   \item{`outbreak`}{Outbreak response data}
#'   \item{`orq_pre`}{Pre-campaign quality assessments}
#'   \item{`orq_intra`}{Intra-campaign monitoring data}
#'   \item{`orq_post`}{Post-campaign evaluation data}
#' }
#'
#' **Health systems & final (1.5-1.6):**
#' \describe{
#'   \item{`dhs`}{DHS survey data}
#'   \item{`final_data`}{Final analysis-ready datasets}
#' }
#'
#' **Outputs (03_outputs):**
#' \describe{
#'   \item{`val`, `val_fig`, `val_tbl`}{Validation outputs}
#'   \item{`interm`, `interm_fig`, `interm_tbl`}{Intermediate products}
#'   \item{`final`, `final_fig`, `final_tbl`, `final_ppt`}{Final outputs}
#' }
#'
#' @section Side Effects:
#' Sets the global option `polio.paths` to the returned list for easy retrieval
#' via `getOption("polio.paths")`.
#'
#' @param base_path Character path to project root. If NULL (default), tries
#'   to auto-detect the root directory.
#' @param quiet Logical; if TRUE, suppresses warnings about missing folders.
#'   Default FALSE.
#'
#' @return Named list of absolute paths (see Path Keys section).
#' @export
#'
#' @examples
#' # typical usage at start of script
#' tmp <- tempdir()
#' initialize_polio_project(tmp)
#' paths <- setup_polio_project_paths(base_path = tmp, quiet = TRUE)
#'
#' # access paths by name
#' paths$admin_shp
#' paths$es
#' paths$final_ppt
#'
#' # build file paths
#' fs::path(paths$afp, "raw", "afp_cases_2024.csv")
#'
#' # retrieve later via option
#' getOption("polio.paths")$cache
setup_polio_project_paths <- function(base_path = NULL, quiet = FALSE) {
  # resolve root
  root <- NULL

  if (is.null(base_path)) {
    # prefer here::here() if available
    if (requireNamespace("here", quietly = TRUE)) {
      root <- normalizePath(here::here(), winslash = "/", mustWork = FALSE)
    }
    # fallback to rprojroot heuristics
    if (
      is.null(root) &&
        requireNamespace("rprojroot", quietly = TRUE)
    ) {
      crit <- rprojroot::has_file(".here") |
        rprojroot::is_git_root
      root <- try(rprojroot::find_root(crit), silent = TRUE)
      if (inherits(root, "try-error")) {
        root <- NULL
      }
      if (!is.null(root)) {
        root <- normalizePath(root, winslash = "/", mustWork = FALSE)
      }
    }
    # final fallback
    if (is.null(root)) {
      root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    }
  } else {
    root <- normalizePath(base_path, winslash = "/", mustWork = FALSE)
  }

  # helper to join paths
  jp <- function(...) fs::path(root, ...)

  # build path tree
  paths <- list(
    core = root,
    # 1.1_foundational
    admin_shp = jp("01_data", "1.1_foundational", "1.1a_admin_boundaries"),
    physical_feat = jp("01_data", "1.1_foundational", "1.1b_physical_features"),
    settlements = jp(
      "01_data",
      "1.1_foundational",
      "1.1c_masterlist_of_settlements"
    ),
    pop_national = jp(
      "01_data",
      "1.1_foundational",
      "1.1d_population",
      "1.1ei_national"
    ),
    pop_worldpop = jp(
      "01_data",
      "1.1_foundational",
      "1.1d_population",
      "1.1eii_worldpop_rasters"
    ),
    cache = jp("01_data", "1.1_foundational", "1.1e_cache_files"),
    # 1.2_surveillance
    es = jp("01_data", "1.2_surveillance", "1.2a_es"),
    afp = jp("01_data", "1.2_surveillance", "1.2b_afp"),
    esurv = jp("01_data", "1.2_surveillance", "1.2c_esurv"),
    # 1.3_outbreak_response
    outbreak = jp("01_data", "1.3_outbreak_response"),
    # 1.4_outbreak_response_quality
    orq_pre = jp(
      "01_data",
      "1.4_outbreak_response_quality",
      "1.4a_pre_campaign"
    ),
    orq_intra = jp(
      "01_data",
      "1.4_outbreak_response_quality",
      "1.4b_intra_campaign"
    ),
    orq_post = jp(
      "01_data",
      "1.4_outbreak_response_quality",
      "1.4c_post_campaign"
    ),
    # 1.5_health_systems
    dhs = jp("01_data", "1.5_health_systems", "1.5a_dhs"),
    # 1.6_final
    final_data = jp("01_data", "1.6_final"),
    # outputs - parent directories
    val = jp("03_outputs", "3.1_validation"),
    interm = jp("03_outputs", "3.2_intermediate_products"),
    final = jp("03_outputs", "3.3_final_outputs"),
    # outputs - figures and tables
    val_fig = jp("03_outputs", "3.1_validation", "figures"),
    val_tbl = jp("03_outputs", "3.1_validation", "tables"),
    interm_fig = jp("03_outputs", "3.2_intermediate_products", "figures"),
    interm_tbl = jp("03_outputs", "3.2_intermediate_products", "tables"),
    final_fig = jp("03_outputs", "3.3_final_outputs", "figures"),
    final_tbl = jp("03_outputs", "3.3_final_outputs", "tables"),
    final_ppt = jp("03_outputs", "3.3_final_outputs", "powerpoint")
  )

  # check directories exist and warn if not
  missing_paths <- character(0)
  for (nm in names(paths)) {
    p <- paths[[nm]]
    if (!fs::dir_exists(p)) {
      missing_paths <- c(missing_paths, nm)
    }
  }

  if (length(missing_paths) > 0 && !quiet) {
    cli::cli_alert_warning(
      "Missing {length(missing_paths)} path{?s}: {.val {missing_paths}}"
    )
    cli::cli_alert_info(
      "Run {.fn initialize_polio_project} to create the folder structure"
    )
  }

  # set option for easy retrieval
  options(polio.paths = paths)

  paths
}
