# poliprep 1.5.0

## New features

- `read()` and `save()` now handle `.qs2` files, using the optional 'qs2' package.
- Added `write_polio_data()`, which writes one object to several file formats under a standard name, with atomic writes, optional date or version tags and pruning of older versions.
- Added `pct_fmt()`, `pluralise()`, `squish_ws()`, `slugify()`, `norm_name_key()` and `norm_guid()` for formatting numbers and text and for building join keys.
- Added `iso_week_monday()`, `semester_start()`, `semester_end()`, `semester_label()`, `last_semesters()`, `format_long_date()` and `format_date_window()` for reporting dates.
- Added `hash_table()`, `hash_geometry()` and `hash_files()` to build cache keys from the contents of tables, boundaries and files.
- Added `stamp_source()`, `read_stamps()`, `data_as_of()` and `check_freshness()` to keep a log of when each data source was last refreshed.

## Dependencies

- 'digest' and 'glue' are now imported. 'openxlsx', 'arrow' and 'vctrs' are suggested.

# poliprep 1.4.0

# poliprep 1.3.0

## Improvements

- Refactored `prep_geonames()` to warn users about unsupported levels in the default look-up file.

## Bug Fixes

- Fixed issue where `handle_file_save()` overwrote existing cache data.
- Resolved incorrect merging behavior in `prep_geonames()`.

## Deprecations

- Deprecated `get_multi_ona_data()` in favor of `get_ona_data()`

### Added
- **`init_folders()`**: A new function to create a standardized project folder structure.
  - Automatically generates directories for organizing data, scripts, and outputs.
  - Ensures consistency and reproducibility in data-related workflows.
  - Provides clear feedback messages using `cli` for successful and existing folders.

#### Example Usage
```r
# Create the project folder structure in the current working directory
init_folders()

# Specify a custom directory
init_folders(base_path = "~/my_project")
```
