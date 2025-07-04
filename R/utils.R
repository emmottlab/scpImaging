# Utils - various utility functions for package for checking on data processing or handling dataframe string reformatting

# 1. generateImageColumns() status: functional
# 2. generateImageColumnsSCE() status: untested but present - wrapper for the above to add these columns to the colData of a singlecellexperiment object
# 3. cleanFilenames(): status: non-existant - removes =hyperlink(" and ") from filenames.

###################################################################################################
#' generateImageColumns(): Generate Derived Image File Columns
#'
#' @description
#' Takes an input data frame and adds new columns containing derived filenames
#' based on a specified source column (e.g., cropped images, masks, overlays) .
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
#' and an optional suffix rule applied before the original extension. Used to generate
#' the processed filenames in scpImaging workflows.
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
#' `column_configs` does not match a default, it defines a new derived column. Recommend
#' **not** overriding the defaults.
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

    # Initialize with source valuen
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

    # Apply suffix rule
    is_replace_rule <-!is.na(suffix_rule) && stringr::str_detect(suffix_rule, "^replace ")

    if (!is_replace_rule &&!is.na(suffix_rule) && nzchar(suffix_rule)) {
      basenames_noext <- stringr::str_c(basenames_noext, suffix_rule)
    }

    # Reconstruct the path
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

  # Add new columns to the original data frame
  if (length(new_cols_data) > 0) {
    original_names <- names(data)
    new_names <- names(new_cols_data)
    clashes <- intersect(original_names, new_names)
    if (length(clashes) > 0) {
      warning(paste("New column names clash with existing columns and will overwrite them:", paste(clashes, collapse=", ")), call. = FALSE)
    }

    data <- dplyr::mutate(data,!!!rlang::list2(!!!new_cols_data))
  }

  return(data)
}

###################################################################################################
# -------------------------------------------------------------------------
# Internal helper function to process colData
# -------------------------------------------------------------------------
.process_colData_for_image_columns <- function(x,
                                               df_processing_function,
                                               ...) {
  current_colData <- SummarizedExperiment::colData(x)

  if (nrow(current_colData) == 0) {
    obj_class <- class(x)[1] # Get the primary class of the object
    warning(paste0(
      "colData of the '", obj_class,
      "' object is empty. No columns will be generated."
    ), call. = FALSE)
    return(x)
  }

  # Preserve original row names from the DataFrame
  original_rownames <- rownames(current_colData)

  # Convert DataFrame to data.frame for the processing function
  colData_df <- as.data.frame(current_colData)

  # Ensure rownames are correctly set on the data.frame if they existed
  if (!is.null(original_rownames) && !identical(rownames(colData_df), original_rownames)) {
    if (length(original_rownames) == nrow(colData_df)) {
      rownames(colData_df) <- original_rownames
    } else {
      # This case should be rare if current_colData had valid rownames
      warning("Rownames from colData could not be consistently applied to the intermediate data.frame.", call. = FALSE)
    }
  }

  # Capture additional arguments for the df_processing_function
  additional_args <- list(...)
  all_args_for_df_func <- c(list(data = colData_df), additional_args)

  # Call the data.frame processing function (i.e., your original generateImageColumns)
  modified_colData_df <- do.call(df_processing_function, all_args_for_df_func)

  # Convert the modified data.frame back to a DataFrame
  # Preserve rownames: assume df_processing_function preserves them,
  # or fall back to original_rownames if structure is compatible.
  final_rownames <- rownames(modified_colData_df)
  if (is.null(final_rownames) && !is.null(original_rownames) && length(original_rownames) == nrow(modified_colData_df)) {
    # Fallback if rownames were lost and dimensions match
    final_rownames <- original_rownames
  } else if (!identical(final_rownames, original_rownames) && !is.null(original_rownames)  && length(original_rownames) == nrow(modified_colData_df)) {
    # If rownames were changed by df_processing_function but originals are still valid and preferred by SE/QFeatures
    # Typically, we want the rownames from modified_colData_df as they might be reordered.
    # However, for colData, the original sample order is paramount.
    # The original `generateImageColumns` is designed to preserve row order and names.
    final_rownames <- original_rownames # Prioritize original sample order
  }

  SummarizedExperiment::colData(x) <- S4Vectors::DataFrame(modified_colData_df, row.names = final_rownames)
  return(x)
}


#' @name generateImageColumnsSCE
#' @rdname generateImageColumnsSCE
#'
#' @title 'generateImageColumnsSCE():' Apply generateImageColumns to Bioconductor Object colData
#'
#' @description
#' A wrapper function to apply \code{generateImageColumns} (a function that processes
#' data.frames) to the \code{colData} of a \code{SummarizedExperiment}
#' (which includes \code{SingleCellExperiment}) or a \code{QFeatures} object.
#' It extracts the \code{colData}, processes it, and then updates the object's
#' \code{colData} with the new columns.
#'
#' @param x A \code{SummarizedExperiment}, \code{SingleCellExperiment}, or \code{QFeatures} object.
#' @param ... Arguments to be passed to the underlying \code{generateImageColumns} function
#'   that operates on data.frames. This typically includes \code{source_column}
#'   and \code{column_configs}.
#'
#' @details
#' This function serves as a wrapper. The method for \code{SummarizedExperiment}
#' also handles \code{SingleCellExperiment} objects due to class inheritance.
#' For \code{QFeatures} objects, this function modifies the primary (global) \code{colData}
#' of the \code{QFeatures} object.
#'
#' Refer to the documentation of the original \code{generateImageColumns} function
#' for details on its arguments and behavior when processing the data.frame.
#'
#' @return The input object (\code{x}) with its \code{colData} updated to include
#' the new columns.
#'
#' @seealso \code{\link{generateImageColumns}} (for the underlying data.frame implementation)
#'
#' @examples
#' # --- Prerequisite: Define or load your original generateImageColumns function ---
#' # generateImageColumns <- function(data, source_column = "ImageFile", ...) {
#' #   # ... (your data.frame processing logic)
#' #   data$newCol <- paste0("processed_", data[[source_column]])
#' #   return(data)
#' # }
#'
#' # --- Example for SummarizedExperiment (and by extension SingleCellExperiment) ---
#' library(SummarizedExperiment)
#' counts <- matrix(rnorm(40), ncol = 4)
#' rownames(counts) <- paste0("gene", 1:10)
#' colnames(counts) <- paste0("sample", 1:4)
#' sample_df <- S4Vectors::DataFrame(
#'   ImageFile = c("s1.png", "s2.png", "s3.png", "s4.png"),
#'   row.names = colnames(counts)
#' )
#' se <- SummarizedExperiment(assays = list(counts = counts), colData = sample_df)
#'
#' # Assuming generateImageColumns and generateImageColumnsSCE are defined:
#' # print(colData(se))
#' # se_modified <- generateImageColumnsSCE(se, source_column = "ImageFile")
#' # print(colData(se_modified))
#'
#' # If sce is a SingleCellExperiment, it would work the same:
#' # library(SingleCellExperiment)
#' # sce <- as(se, "SingleCellExperiment")
#' # sce_modified <- generateImageColumnsSCE(sce, source_column = "ImageFile")
#' # print(colData(sce_modified))
#'
#' # --- Example for QFeatures ---
#' library(QFeatures)
#' qf_coldata <- S4Vectors::DataFrame(
#'   GlobalImageFile = c("q_s1.jpg", "q_s2.jpg"),
#'   Batch = c(1,2),
#'   row.names = c("qSample1", "qSample2")
#' )
#' # Creating a minimal QFeatures object for example
#' qf <- QFeatures(colData = qf_coldata) # No assays needed for this colData example
#'
#' # print(colData(qf))
#' # qf_modified <- generateImageColumnsSCE(qf, source_column = "GlobalImageFile")
#' # print(colData(qf_modified))
#'
#' @importMethodsFrom SummarizedExperiment colData
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom QFeatures QFeatures
#' @import S4Vectors
NULL

#' @rdname generateImageColumnsSCE
#' @export
setGeneric("generateImageColumnsSCE", function(x, ...) standardGeneric("generateImageColumnsSCE"))

#' @rdname generateImageColumnsSCE
#' @export
setMethod("generateImageColumnsSCE", "SummarizedExperiment", # Also covers SingleCellExperiment
          function(x, ...) {
            .process_colData_for_image_columns(x, generateImageColumns, ...)
          })

#' @rdname generateImageColumnsSCE
#' @export
setMethod("generateImageColumnsSCE", "QFeatures",
          function(x, ...) {
            .process_colData_for_image_columns(x, generateImageColumns, ...)
          })

###################################################################################################
#' @title 'cleanFilenames()': Clean Hyperlink Formatting from DataFrame Columns
#'
#' @description This function removes specified prefix and suffix patterns,
#' typically hyperlink formatting (e.g., from Excel), from character columns
#' within a data frame.
#'
#' @param data A \code{data.frame} containing the columns to be cleaned.
#' @param column_names A character vector specifying the names of the columns
#'   in \code{data} that need to be cleaned.
#' @param prefix_pattern A character string containing a regular expression
#'   for the prefix to be removed from the start of the strings in the
#'   specified columns. Defaults to \code{'^=HYPERLINK\\\\("'}.
#' @param suffix_pattern A character string containing a regular expression
#'   for the suffix to be removed from the end of the strings in the
#'   specified columns. Defaults to \code{'"\\\\)$'}.
#'
#' @return A \code{data.frame} with the specified formatting removed from the
#'   target columns. Columns not specified or not of character type (after a
#'   warning) will remain unchanged.
#'
#' @details
#' The function iterates through each column specified in \code{column_names}.
#' For each column:
#' \enumerate{
#'   \item It first checks if the column is of character type. If not, a warning
#'         is issued for that column, and it is skipped (returned as is).
#'   \item If it is a character column, it sequentially applies two \code{gsub}
#'         operations:
#'         \itemize{
#'           \item Removes the \code{prefix_pattern} from the beginning of
#'                 each string.
#'           \item Removes the \code{suffix_pattern} from the end of each string
#'                 (from the result of the prefix removal).
#'         }
#' }
#' The default patterns are designed to clean strings like
#' \code{"=HYPERLINK(\\"path/to/file.jpg\\")"} into \code{"path/to/file.jpg"}.

#'
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   cDat_example <- data.frame(
#'     ID = 1:3,
#'     ImageFile = c('=HYPERLINK("image1.png")',
#'                   'plain_file.jpg',
#'                   '=HYPERLINK("image2.tif")'),
#'     Background = c('=HYPERLINK("bg1.png")',
#'                    '=HYPERLINK("bg2.jpg")',
#'                    'no_hyperlink_here'),
#'     Notes = c("Note1", "=HYPERLINK(\"doc1.pdf\")", "Note3"),
#'     NumericCol = c(10,20,30),
#'     FactorCol = factor(c("A", "B", "A")),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   # Clean 'ImageFile' and 'Background' columns with default patterns
#'   cleaned_data1 <- cleanFilenames(cDat_example,
#'                                   column_names = c("ImageFile", "Background"))
#'   print("Cleaned ImageFile and Background:")
#'   print(cleaned_data1)
#'
#'   # Clean 'Notes' column with default patterns
#'   cleaned_data_notes <- cleanFilenames(cDat_example, column_names = "Notes")
#'   print("Cleaned Notes:")
#'   print(cleaned_data_notes)
#'
#' }
#'#' @importFrom dplyr mutate across all_of
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate across all_of cur_column
#' @export
cleanFilenames <- function(data, column_names,
                           prefix_pattern = '^=HYPERLINK\\("',
                           suffix_pattern = '"\\)$') {

  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame.", call. = FALSE)
  }

  if (missing(column_names) || !is.character(column_names) || length(column_names) == 0) {
    stop("'column_names' must be provided as a non-empty character vector of column names.", call. = FALSE)
  }

  # Ensure all elements in column_names are single, non-NA, non-empty strings
  if (any(!sapply(column_names, function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)))) {
    stop("All elements in 'column_names' must be single, non-NA, non-empty character strings.", call. = FALSE)
  }

  missing_cols <- column_names[!column_names %in% names(data)]
  if (length(missing_cols) > 0) {
    stop(paste("The following specified columns are not in the data frame:",
               paste(shQuote(missing_cols), collapse = ", ")), call. = FALSE)
  }

  if (!is.character(prefix_pattern) || length(prefix_pattern) != 1) {
    stop("'prefix_pattern' must be a single string (regular expression).", call. = FALSE)
  }
  if (!is.character(suffix_pattern) || length(suffix_pattern) != 1) {
    stop("'suffix_pattern' must be a single string (regular expression).", call. = FALSE)
  }

  # Check for dplyr availability for robustness if run outside of a package context
  # For package use, @importFrom and DESCRIPTION's Imports field handle this.
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required for this function. Please install it.", call. = FALSE)
  }

  # --- Cleaning Logic ---
  data_modified <- data %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(column_names), .fns = function(current_col_vector) {
        # Check if the column is of character type. Factors are not directly character.
        if (!is.character(current_col_vector)) {
          warning(paste0("Column '", dplyr::cur_column(),
                         "' is not of character type (e.g., it might be factor, numeric). ",
                         "Values in this column will not be cleaned."),
                  call. = FALSE)
          return(current_col_vector) # Return the column unchanged
        }

        # Apply prefix removal (gsub is from base R)
        cleaned_vector <- gsub(prefix_pattern, "", current_col_vector)
        # Apply suffix removal on the already prefix-cleaned vector
        cleaned_vector <- gsub(suffix_pattern, "", cleaned_vector)

        return(cleaned_vector)
      })
    )

  return(data_modified)
}
