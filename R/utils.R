# Utils - various utility functions for package for checking on data processing or handling dataframe string reformatting

# 1. generateImageColumns() status: functional
# 2. generateImageColumnsSCP() status: non-existant

###################################################################################################
#' generateImageColumns(): Generate Derived Image File Columns
#'
#' @description
#' Takes an input data frame and adds new columns containing derived filenames
#' based on a specified source column. The derivation follows configurable
#' prefix and suffix rules, primarily designed for managing related image files
#' (e.g., cropped images, masks, overlays) in bioinformatics workflows.
#'
#' @param data A `data.frame` or `tibble` containing the source image file information.
#' @param source_column Character string. The name of the column in `data` that
#'   contains the source filenames (e.g., image paths). Defaults to "ImageFile".
#' @param column_configs An optional named list to override or add custom column
#'   configurations. Each element of the list should be named (e.g., "Cropped",
#'   "CP_Mask") and contain a sub-list with elements `col_name` (character, the
#'   desired name for the new column), `prefix` (character, prefix to add,
#'   can be `NA` or empty string for none), and `suffix` (character, suffix rule,
#'   can be `NA` or empty string for none). Suffix rules starting with "replace "
#'   (e.g., "replace.png with _mask.png") trigger string replacement; otherwise,
#'   the suffix is appended before the original extension. Defaults to `NULL`,
#'   using the internal default configurations.
#'
#' @details
#' The function iterates through a set of predefined or user-provided configurations
#' to generate new columns. Each configuration specifies the new column's name,
#' an optional prefix to prepend to the source filename's base name (preserving path and extension),
#' and an optional suffix rule applied before the original extension.
#'
#' Default Configurations:
#' * **Cropped**: Adds column "Cropped" with prefix "cropped_" applied to the base filename.
#' * **CP_Mask**: Adds column "CP_Mask" with prefix "cropped_" applied to the base filename and replaces the ".png" extension/suffix within the entire string with "_cp_masks.png".
#' * **CP_Overlay**: Adds column "CP_Overlay" with prefix "cropped_" applied to the base filename and replaces the ".png" extension/suffix within the entire string with "_cp_masks_overlay.png".
#' * **CP_Overlay_Parent**: Adds column "CP_Overlay_Parent" appending the suffix "_parent_overlayed" before the original file extension.
#'
#'
#' The `column_configs` argument allows overriding these defaults. The names of the
#' list provided in `column_configs` should match the keys of the default configurations
#' (e.g., "Cropped", "CP_Mask") to override them. If a configuration name in
#' `column_configs` does not match a default, it defines a new derived column.
#'
#' Prefix rules add the specified string after any directory path but before the base filename.
#'
#' Suffix rules are applied as follows:
#' \itemize{
#'   \item If the `suffix` string starts with "replace ", the function attempts to
#'     perform a string replacement on the *entire* generated string (after prefixing). The format expected is "replace <old_string> with <new_string>".
#'     For example, "replace.png with _mask.png". The replacement uses fixed string matching, not regular expressions.
#'   \item Otherwise, the `suffix` string is appended to the filename's base name (after the prefix, if any) but *before* the original file extension.
#' }
#'
#' If the source filename is `NA`, the resulting derived filenames will also be `NA`.
#' If a "replace" suffix rule is specified but the `<old_string>` is not found in a
#' given source filename, the replacement does not occur for that filename, and a single
#' warning summarizing all mismatches may be issued at the end.
#' Generated column names that clash with existing columns in `data` will overwrite them, issuing a warning.
#'
#' The function uses `dplyr::mutate` for adding columns and `stringr` functions
#' (`str_c`, `str_replace`, `str_detect`, `str_match`) along with `tools` functions
#' (`file_path_sans_ext`, `file_ext`) for efficient string and path manipulation.
#'
#' @returns The input `data` frame (`tibble`) with the newly generated columns added.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   SampleID = 1:3,
#'   ImageFile = c(
#'     "path/subpath/458_Printed_T1_F1_R62_C19_(A-1)_Trans_samplename_Run.png",
#'     "another_image_file.tif",
#'     NA
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Basic usage with defaults
#' result_default <- generateImageColumns(df)
#' print(result_default)
#'
#' # Example with a different source column
#' df2 <- data.frame(
#'   ID = 1,
#'   OriginalPath = "path/to/image.png",
#'   stringsAsFactors = FALSE
#' )
#' result_source <- generateImageColumns(df2, source_column = "OriginalPath")
#' print(result_source)
#'
#' # Example overriding defaults and adding a new column
#' custom_configs <- list(
#'   Cropped = list(col_name = "MyCropped", prefix = "mycrop_", suffix = NA),
#'   CP_Mask = list(col_name = "MaskFile", prefix = NA, suffix = "replace.png with _MASK.tif"),
#'   NewCol = list(col_name = "ExtraInfo", prefix = "info_", suffix = "_extra") # New column
#' )
#' result_custom <- generateImageColumns(df, column_configs = custom_configs)
#' print(result_custom)
#'
#' # Example with suffix replacement mismatch (no ".png" in source)
#' df_mismatch <- data.frame(
#'   ImageFile = "image_no_png_suffix.jpeg",
#'   stringsAsFactors = FALSE
#' )
#' # CP_Mask and CP_Overlay rules won't replace ".png" and may trigger a warning
#' result_mismatch <- generateImageColumns(df_mismatch)
#' print(result_mismatch)
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_c str_replace str_detect str_match fixed
#' @importFrom rlang := sym list2
#' @importFrom utils modifyList
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @export
generateImageColumns <- function(data,
                                 source_column = "ImageFile",
                                 column_configs = NULL) {

  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.character(source_column) || length(source_column)!= 1) {
    stop("'source_column' must be a single character string.", call. = FALSE)
  }
  if (!source_column %in% names(data)) {
    stop(paste0("Source column '", source_column, "' not found in data."), call. = FALSE)
  }
  if (!is.character(data[[source_column]]) &&!is.factor(data[[source_column]])) {
    warning(paste0("Source column '", source_column, "' is not character or factor. Coercing to character."), call. = FALSE)
    # Coercion happens explicitly below
  }

  # Validate column_configs structure if provided
  if (!is.null(column_configs)) {
    if (!is.list(column_configs) || is.null(names(column_configs)) || any(names(column_configs) == "")) {
      stop("'column_configs' must be a non-empty named list.", call. = FALSE)
    }
    valid_structure <- all(sapply(column_configs, function(conf) {
      is.list(conf) &&
        all(c("col_name", "prefix", "suffix") %in% names(conf)) &&
        is.character(conf$col_name) && length(conf$col_name) == 1 &&
        (is.na(conf$prefix) || (is.character(conf$prefix) && length(conf$prefix) == 1)) &&
        (is.na(conf$suffix) || (is.character(conf$suffix) && length(conf$suffix) == 1))
    }))
    if (!valid_structure) {
      stop("Each element in 'column_configs' must be a list containing 'col_name' (character), 'prefix' (character/NA), and 'suffix' (character/NA).", call. = FALSE)
    }
  }

  # --- Default Configuration ---
  default_configs <- list(
    Cropped = list(col_name = "Cropped", prefix = "cropped_", suffix = NA),
    CP_Mask = list(col_name = "CP_Mask", prefix = "cropped_", suffix = "_cp_masks"),
    CP_Overlay = list(col_name = "CP_Overlay", prefix = "cropped_", suffix = "_cp_masks_overlay"),
    CP_Overlay_Parent = list(col_name = "CP_Overlay_Parent", prefix = NA, suffix = "_parent_overlayed") # Suffix applied before ext
  )

  # --- Merge Configurations ---
  final_configs <- default_configs
  if (!is.null(column_configs)) {
    final_configs <- utils::modifyList(default_configs, column_configs, keep.null = TRUE)
    new_keys <- setdiff(names(column_configs), names(default_configs))
    if(length(new_keys) > 0) {
      final_configs <- c(final_configs, column_configs[new_keys])
    }
  }


  # --- Generate Columns ---
  source_values <- as.character(data[[source_column]]) # Ensure character
  new_cols_data <- list()
  warnings_suffix_list <- character() # To collect unique warnings about suffix replacement

  for (config_name in names(final_configs)) {
    config <- final_configs[[config_name]]
    new_col_name <- config$col_name
    prefix <- config$prefix
    suffix_rule <- config$suffix

    # Initialize with source values for this iteration
    current_col_values <- source_values
    is_na_source <- is.na(source_values)

    # Separate path, basename, extension for manipulation
    paths <- dirname(current_col_values)
    basenames <- basename(current_col_values)
    basenames_noext <- tools::file_path_sans_ext(basenames)
    exts <- tools::file_ext(basenames)
    exts_with_dot <- ifelse(nzchar(exts), paste0(".", exts), "")

    # Apply prefix to basename
    if (!is.na(prefix) && nzchar(prefix)) {
      basenames_noext <- stringr::str_c(prefix, basenames_noext)
    }

    # Apply suffix rule (append before extension, unless it's a replace rule)
    is_replace_rule <-!is.na(suffix_rule) && stringr::str_detect(suffix_rule, "^replace ")

    if (!is_replace_rule &&!is.na(suffix_rule) && nzchar(suffix_rule)) {
      basenames_noext <- stringr::str_c(basenames_noext, suffix_rule)
    }

    # Reconstruct the path (without replacement rule yet)
    modified_basenames <- stringr::str_c(basenames_noext, exts_with_dot)
    current_col_values <- ifelse(paths == ".", modified_basenames, file.path(paths, modified_basenames))
    current_col_values[is_na_source] <- NA # Ensure NA propagation

    # Apply "replace" suffix rule to the *entire* constructed string
    if (is_replace_rule) {
      parts <- stringr::str_match(suffix_rule, "^replace (.*) with (.*)$")
      if (!is.na(parts)) { # Check if regex matched
        old_str <- parts
        new_str <- parts

        # Perform replacement only where old_str exists and value is not NA
        needs_replace_mask <-!is_na_source & stringr::str_detect(current_col_values, stringr::fixed(old_str))
        current_col_values[needs_replace_mask] <- stringr::str_replace(
          current_col_values[needs_replace_mask],
          stringr::fixed(old_str),
          new_str
        )

        # Check for mismatches (where replacement was intended but old_str wasn't found in non-NA strings)
        mismatches_mask <-!is_na_source &!needs_replace_mask
        if(any(mismatches_mask)) {
          warning_msg <- paste0("Suffix rule '", suffix_rule, "' for column '", new_col_name,
                                "' did not find '", old_str, "' in some non-NA source filenames.")
          warnings_suffix_list <- c(warnings_suffix_list, warning_msg)
        }
      } else {
        warning(paste0("Invalid 'replace' suffix rule format for column '", new_col_name, "': ", suffix_rule), call. = FALSE)
      }
    }

    # Store results for this column, ensuring NAs are preserved
    new_cols_data[[new_col_name]] <- current_col_values
  }

  # Issue a single warning for unique suffix mismatches if any occurred
  if(length(warnings_suffix_list) > 0) {
    unique_warnings <- unique(warnings_suffix_list)
    warning("Suffix replacement issues detected:\n- ", paste(unique_warnings, collapse="\n- "), call. = FALSE)
  }

  # Add new columns to the original data frame using dplyr::mutate
  if (length(new_cols_data) > 0) {
    original_names <- names(data)
    new_names <- names(new_cols_data)
    clashes <- intersect(original_names, new_names)
    if (length(clashes) > 0) {
      warning(paste("New column names clash with existing columns and will overwrite them:", paste(clashes, collapse=", ")), call. = FALSE)
    }
    # Use dynamic dots with := via list2 for compatibility and clarity
    data <- dplyr::mutate(data,!!!rlang::list2(!!!new_cols_data))
  }

  return(data)
}
