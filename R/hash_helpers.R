#' Hash the Contents of a Table
#'
#' Returns a hash of the named columns of a table, for use as a cache key.
#' Rows are sorted before hashing, so the same records read in a different
#' order give the same hash. A row count alone does not establish that two
#' tables hold the same records.
#'
#' @param data A data.frame or tibble.
#' @param cols Character vector naming the columns to hash.
#'
#' @return A single hash string.
#'
#' @examples
#' df <- data.frame(id = c("b", "a"), n = c(2, 1))
#' hash_table(df, c("id", "n")) == hash_table(df[2:1, ], c("id", "n"))
#'
#' @export
hash_table <- function(data, cols) {
  missing <- setdiff(cols, names(data))

  if (length(missing) > 0) {
    cli::cli_abort("Column{?s} {.val {missing}} not in the table to hash")
  }

  keyed <- lapply(as.data.frame(data)[, cols, drop = FALSE], as.character)

  ord <- order(do.call(paste, c(keyed, sep = "\r")))

  digest::digest(lapply(keyed, \(x) x[ord]))
}

#' Hash a Set of Boundaries
#'
#' Returns a hash of the geometry of an `sf` object and its identifiers, for
#' use as a cache key. Features are sorted by identifier before hashing. A
#' redrawn boundary changes the hash even when the identifiers do not change.
#'
#' @param boundaries An `sf` object.
#' @param id_col Name of the column that identifies each feature.
#'
#' @return A single hash string.
#'
#' @examples
#' pts <- sf::st_sf(
#'   id = c("a", "b"),
#'   geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
#' )
#' hash_geometry(pts, "id")
#'
#' @export
hash_geometry <- function(boundaries, id_col) {
  ord <- order(as.character(boundaries[[id_col]]))

  digest::digest(list(
    ids = as.character(boundaries[[id_col]])[ord],
    wkb = sf::st_as_binary(sf::st_geometry(boundaries)[ord])
  ))
}

#' Hash the Contents of Files
#'
#' Returns one hash for a set of files and folders, for inputs that are
#' neither a table nor geometry, such as a template or a road network.
#' Folders are read recursively. The hash covers each file's contents, size
#' and path relative to `root`.
#'
#' @param paths Character vector of file and folder paths. Paths that do not
#'   exist are ignored.
#' @param root Folder the file paths are made relative to before hashing, so
#'   the hash does not depend on where the project sits on disk. Default is
#'   the working directory.
#'
#' @return A single hash string, or `NA` when no file is found.
#'
#' @examples
#' tmp <- tempfile(fileext = ".txt")
#' writeLines("a", tmp)
#' hash_files(tmp, root = tempdir())
#'
#' @export
hash_files <- function(paths, root = getwd()) {
  files <- sort(as.character(
    fs::dir_ls(paths[fs::dir_exists(paths)], recurse = TRUE, type = "file")
  ))

  files <- c(files, sort(as.character(paths[fs::is_file(paths)])))

  if (length(files) == 0) {
    return(NA_character_)
  }

  digest::digest(list(
    names = as.character(fs::path_rel(files, root)),
    sizes = as.numeric(fs::file_info(files)$size),
    hashes = purrr::map_chr(files, \(f) digest::digest(file = f))
  ))
}
