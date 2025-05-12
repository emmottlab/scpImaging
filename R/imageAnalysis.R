### Functions for analysing cellenONE image metadata produced by Cellpose or CellProfiler

# Currently this includes
# 1. countCells() - status: functional - need to tidy
# 2. addCellProfilerAttributes() - status: compiles, untested
# 3. addCellProfilerAttributesSCE() - status: compiles, untested

##############################################################################################################

#' 'countCells()':Count Unique Masks in PNG Segmentation Masks
#'
#' @description
#' Processes PNG segmentation masks generated from cellpose .npy files using the
#' `magick` package to count the number of unique positive integer values.
#' These values correspond to individual segmented objects (e.g., cells),
#' while background pixels (value 0) are ignored.
#' The function can handle input specified as a directory containing masks, an
#' explicit list of mask file paths, or a directory and a filename suffix.
#'
#' @details
#' This function reads PNG files using `magick::image_read`. It then uses
#' `magick::image_data` to extract the raw pixel data for the grayscale
#' channel.
#'
#' Because `magick::image_info` may not reliably return the bit depth for all
#' PNG files, this function uses the `expected_bit_depth` argument to interpret
#' the raw pixel data (bytes):
#' - If `expected_bit_depth` is 8, raw bytes directly represent integer values (0-255).
#' - If `expected_bit_depth` is 16, pairs of raw bytes are combined to reconstruct the
#'   16-bit integer values (0-65535), assuming big-endian byte order.
#'
#' The function assumes that the input PNGs are grayscale integer images where
#' different masks are represented by unique positive integers and the
#' background is 0. If non-grayscale images are encountered, a warning is
#' issued, and the function attempts to process the grayscale information.
#'
#' The function includes error handling: if a file cannot be read or processed
#' (e.g., corrupted file, non-PNG format, unsupported expected bit depth),
#' a warning is issued, and `NA` is returned for that file's count.
#'
#' Provide *either* `path` (optionally with `suffix`) *or* `filelist`, but not both.
#'
#' @param path A character string specifying the path to the directory
#'   containing the PNG mask files. Mutually exclusive with `filelist`.
#'   Defaults to `NULL`.
#' @param filelist A character vector of full file paths to the PNG mask files.
#'   Mutually exclusive with `path`. Defaults to `NULL`.
#' @param suffix A character string specifying the filename suffix (pattern)
#'   to match when `path` is provided. The pattern is matched at the end of
#'   the filename. Defaults to `".png"`. Ignored if `filelist` is provided.
#' @param expected_bit_depth A numeric value indicating the expected bit depth
#'   of the original PNG mask files (typically 8 or 16). This is used to
#'   interpret the raw pixel data correctly. Defaults to 8.
#'
#' @return A dataframe with two columns:
#'   \item{FileName}{Character, the base name of the processed PNG file.}
#'   \item{CP_CellCount}{Integer, the number of unique positive integer mask
#'   values found in the image. Contains `NA` if the file could not be
#'   processed.}
#'
#' @importFrom magick image_read image_data image_info image_write
#' @export
#'
#' @examples
#' \dontrun{
#' # Example Setup: Create dummy PNG mask files using the 'magick' package
#' temp_dir <- tempdir()
#' # Mask 1: 2 objects (IDs 1, 2)
#' mask1_data <- matrix(c(0, 0, 1, 1, 0, 2, 2, 0, 0), nrow = 3, byrow = TRUE)
#' # Mask 2: 4 objects (IDs 1, 2, 3, 4)
#' mask2_data <- matrix(c(1, 1, 1, 0, 0, 0, 2, 3, 4), nrow = 3, byrow = TRUE)
#' # Mask 3: 3 objects (IDs 10, 20, 300) - requires 16-bit
#' mask3_data_16bit <- matrix(c(0, 10, 20, 300), nrow = 2, byrow = TRUE)
#'
#' # Create magick images from matrices (scale to 0-1 for image_read)
#' # Save as 16-bit PNGs to handle mask3
#' max_val_16bit <- 65535
#' img1_magick <- magick::image_read(mask1_data / max_val_16bit)
#' img2_magick <- magick::image_read(mask2_data / max_val_16bit)
#' img3_magick <- magick::image_read(mask3_data_16bit / max_val_16bit)
#'
#' # Set depth to 16-bit before writing
#' img1_magick <- magick::image_depth(img1_magick, 16)
#' img2_magick <- magick::image_depth(img2_magick, 16)
#' img3_magick <- magick::image_depth(img3_magick, 16)
#'
#' # Write PNGs using magick
#' magick::image_write(img1_magick,
#'                     path = file.path(temp_dir, "imageA_masks.png"),
#'                     format = "png")
#' magick::image_write(img2_magick,
#'                     path = file.path(temp_dir, "imageB_masks.png"),
#'                     format = "png")
#' magick::image_write(img3_magick,
#'                     path = file.path(temp_dir, "imageC_16bit_masks.png"),
#'                     format = "png")
#'
#' # Create a non-png file to test error handling
#' writeLines("not a png", file.path(temp_dir, "not_a_png.txt"))
#'
#' # --- Usage Examples ---
#'
#' # 1. Using path and default suffix (".png") and default depth (16)
#' counts_path_default <- countCells(path = temp_dir)
#' print(counts_path_default)
#' # Expected output: Counts for A, B, C (2, 4, 3 respectively)
#'
#' # 2. Using path and specific suffix, explicitly setting 16-bit depth
#' counts_path_suffix <- countCells(path = temp_dir,
#'                                             suffix = "_masks.png",
#'                                             expected_bit_depth = 16)
#' print(counts_path_suffix)
#' # Expected output: Counts for A, B, C (2, 4, 3 respectively)
#'
#' # 3. Using filelist (including non-png and non-existent files to show NA)
#' file_paths <- c(
#'   file.path(temp_dir, "imageA_masks.png"),
#'   file.path(temp_dir, "imageB_masks.png"),
#'   file.path(temp_dir, "imageC_16bit_masks.png"),
#'   file.path(temp_dir, "non_existent_file.png"), # Test non-existent file
#'   file.path(temp_dir, "not_a_png.txt")          # Test invalid file
#' )
#' counts_filelist <- countCells(filelist = file_paths,
#'                                          expected_bit_depth = 16)
#' print(counts_filelist)
#' # Expected output: Counts for A, B, C, and NA for the last two with warnings.
#'
#' # 4. Example assuming masks were 8-bit (will misinterpret C)
#' #    (Need to recreate test files as 8-bit for this to be accurate)
#' # counts_8bit <- countCells(path = temp_dir,
#' #                                      suffix = "_masks.png",
#' #                                      expected_bit_depth = 8)
#' # print(counts_8bit)
#'
#' # Clean up temporary files
#' unlink(temp_dir, recursive = TRUE)
#' }
#'
countCells <- function(path = NULL, filelist = NULL, suffix = ".png", expected_bit_depth = 8) {

  # --- Input Validation ---
  stopifnot("Provide either 'path' or 'filelist', but not both." = (!is.null(path) +!is.null(filelist) == 1),
            "If provided, 'path' must be a single character string." = (is.null(path) || (is.character(path) && length(path) == 1)),
            "If provided, 'filelist' must be a character vector." = (is.null(filelist) || is.character(filelist)),
            "'suffix' must be a single character string." = (is.character(suffix) && length(suffix) == 1),
            "'expected_bit_depth' must be a single numeric value." = (is.numeric(expected_bit_depth) && length(expected_bit_depth) == 1),
            "'expected_bit_depth' must be 8 or 16." = (expected_bit_depth %in% c(8, 16)))

  if (!is.null(path)) {
    stopifnot("Provided 'path' does not exist or is not a directory." = dir.exists(path))
  }

  # --- Determine Files to Process ---
  if (!is.null(path)) {
    # Ensure suffix pattern matches end of string, case-insensitive
    pattern_safe <- paste0(gsub("([.^$*+?()[{\\|])", "\\\\\\1", suffix), "$")
    files_to_process <- list.files(path = path, pattern = pattern_safe, full.names = TRUE, ignore.case = TRUE)
    if (length(files_to_process) == 0) {
      warning("No files found in '", path, "' matching suffix '", suffix, "'")
      return(data.frame(FileName = character(0), CP_CellCount = integer(0)))
    }
  } else {
    files_to_process <- filelist
  }

  # --- Process Files ---
  results_list <- vector("list", length(files_to_process))
  names(results_list) <- basename(files_to_process)

  for (i in seq_along(files_to_process)) {
    file_path <- files_to_process[i]
    file_base_name <- basename(file_path)

    count <- tryCatch({
      # Check file existence before attempting to read with magick
      if (!file.exists(file_path)) {
        stop("File not found.")
      }

      # Read image using magick
      img_magick <- magick::image_read(file_path)

      # Get image info (only first frame if multiple) - mainly for width/height now
      info <- magick::image_info(img_magick)[1, ]
      width <- info$width
      height <- info$height
      colorspace <- info$colorspace

      # Check colorspace - warn if not explicitly Gray or Grayscale, but proceed
      if (!is.na(colorspace) &&!colorspace %in% c("Gray", "Grayscale")) {
        warning(paste("Image", file_base_name, "colorspace is", colorspace, "(expected Gray/Grayscale). Proceeding by extracting grayscale channel data."), call. = FALSE)
      }

      # Extract raw grayscale pixel data (returns raw array: width x height x channels)
      img_raw <- magick::image_data(img_magick, channels = 'gray')

      if (is.null(img_raw) || length(img_raw) == 0) {
        stop("Extracted raw pixel data is empty or NULL.")
      }

      # Convert raw data to integer vector based on EXPECTED depth
      img_int_vec <- NULL # Initialize
      if (expected_bit_depth == 8) {
        # Raw bytes directly represent 8-bit integers
        # Check length consistency
        expected_len_8bit <- as.numeric(width) * as.numeric(height) # Ensure numeric calculation
        if (length(img_raw)!= expected_len_8bit) {
          stop(paste("Raw data length", length(img_raw), "mismatch for 8-bit image. Expected", expected_len_8bit))
        }
        img_int_vec <- as.integer(img_raw)
      } else if (expected_bit_depth == 16) {
        # Raw bytes need to be combined for 16-bit integers
        # Check length consistency (2 bytes per pixel)
        expected_len_16bit_raw <- as.numeric(width) * as.numeric(height) * 2 # Ensure numeric calculation
        if (length(img_raw)!= expected_len_16bit_raw) {
          stop(paste("Raw data length", length(img_raw), "mismatch for 16-bit image. Expected", expected_len_16bit_raw))
        }
        raw_bytes <- as.vector(img_raw)
        # Read 16-bit unsigned integers, assuming big-endian (network byte order common in PNG)
        expected_pixels_16bit <- as.numeric(width) * as.numeric(height) # Ensure numeric calculation
        img_int_vec <- readBin(raw_bytes, what = "integer", size = 2, n = expected_pixels_16bit, signed = FALSE, endian = "big")
      } else {
        stop(paste("Unsupported expected_bit_depth:", expected_bit_depth, ". Only 8 or 16 are supported."))
      }

      if (is.null(img_int_vec) || length(img_int_vec) == 0) {
        stop("Failed to convert raw data to integer vector.")
      }

      # Check if the number of pixels matches expected dimensions
      expected_total_pixels <- as.numeric(width) * as.numeric(height) # Ensure numeric calculation
      if (length(img_int_vec)!= expected_total_pixels) {
        stop(paste("Integer pixel vector length mismatch. Expected", expected_total_pixels, "pixels, but got", length(img_int_vec)))
      }

      # Get unique non-zero pixel values (mask IDs)
      unique_values <- unique(img_int_vec)
      if (length(unique_values) == 0 && length(img_int_vec) > 0) {
        # This case might happen if all pixels are the same (e.g., all 0)
        # but unique() should still return that one value. Check if img_int_vec was valid.
        warning(paste("unique() returned empty set for", file_base_name, "despite non-empty input vector. Check pixel values."), call. = FALSE)
        mask_ids <- integer(0) # Empty set of mask IDs
      } else {
        mask_ids <- setdiff(unique_values, 0) # Exclude background (0)
      }

      # Return the count of unique positive mask IDs
      length(mask_ids)

    }, error = function(e) {
      warning(paste("Failed to process file:", file_base_name, "- Error:", conditionMessage(e)), call. = FALSE)
      return(NA_integer_) # Return NA for the count on error
    }) # end tryCatch

    results_list[[i]] <- count
  } # end for loop

  # --- Format Output ---
  output_df <- data.frame(
    FileName = names(results_list),
    CP_CellCount = unlist(results_list),
    row.names = NULL # Ensure row names are default integers
  )

  return(output_df)
}

##############################################################################################################
#' 'addCellProfilerAttributes()': Add Attributes from a Second Data Frame Based on Keys
#'
#' Merges attributes (columns) from a second data frame (`df2`) into a primary
#' data frame (`df1`) based on specified key columns. This function performs a
#' left join, keeping all rows from `df1`. It provides options for handling
#' cases where keys in `df2` represent multiple measurements (multiplets).
#'
#' @details
#' The function uses key columns `key1_col` from `df1` and `key2_col` from `df2`
#' to match rows. All rows from `df1` are preserved.
#'
#'   Handling of multiple rows for the same key (multiplets) in `df2`:
#'   - `voidmultiplets` (default): If a key in `key2_col` appears in multiple
#'     rows of `df2`, the corresponding rows in the merged output will have `NA`
#'     values for all columns originating from `df2`.
#'   - `takemaxarea`: If a key in `key2_col` appears in multiple rows of `df2`,
#'     only the data from the single `df2` row having the maximum value in the
#'     `area_col` column will be added to `df1`, i.e. the largest cell. If the values are
#'     identical, will take the first encountered row among those with the maximum area.
#'
#' @param df1 The primary data frame (data.frame or tibble). All rows from this
#'   data frame will be kept.
#' @param df2 The secondary data frame (data.frame or tibble) containing the
#'   attributes to add.
#' @param key1_col The name of the key column in `df1` (character string).
#'   Defaults to `"Cropped_Path"`.
#' @param key2_col The name of the key column in `df2` (character string).
#'   Defaults to `"FileName_Image"`.
#' @param multiplet_handling Method to handle multiple rows for the same key in `df2`.
#'   Must be one of `"voidmultiplets"` or `"takemaxarea"` (character string).
#'   Defaults to `"voidmultiplets"`.
#' @param area_col The name of the column in `df2` containing the area values,
#'   required only if `multiplet_handling = "takemaxarea"` (character string).
#'   Defaults to `"AreaShape_Area"`.
#'
#' @return A data frame containing all rows and columns from `df1`, augmented
#'   with columns from `df2` based on the matched keys and the specified
#'   `multiplet_handling` strategy. Columns added from `df2` will exclude the
#'   key column (`key2_col`).
#'
#' @examples
#' # --- Sample Data ---
#' df1 <- data.frame(
#'   ID = 1:5,
#'   Cropped_Path = c("path/A", "path/B", "path/C", "path/D", "path/E"),
#'   OtherData1 = rnorm(5)
#' )
#'
#' df2 <- data.frame(
#'   FileName_Image = c("path/A", "path/B", "path/B", "path/C", "path/C", "path/F"),
#'   AttributeX = letters[1:6],
#'   AreaShape_Area = c(100, 150, 120, 200, 250, 50),
#'   AttributeY = LETTERS[1:6]
#' )
#'
#' # --- Usage Examples ---
#'
#' # Example 1: Default behavior ('voidmultiplets')
#' merged_void <- addCellProfilerAttributes(df1, df2)
#' print(merged_void)
#'
#' # Example 2: Using 'takemaxarea' to resolve multiplets ## Changed value in comment
#' # For "path/B", the row with AreaShape_Area = 150 is kept.
#' # For "path/C", the row with AreaShape_Area = 250 is kept.
#' merged_take_max <- addCellProfilerAttributes(df1, df2,
#'                                         multiplet_handling = "takemaxarea") ## Changed value here
#' print(merged_take_max)
#'
#'
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr %>% left_join group_by slice_max filter n select distinct anti_join bind_rows rename_with
addCellProfilerAttributes <- function(df1,
                                      df2,
                                      key1_col = "Cropped_Path",
                                      key2_col = "FileName_Image",
                                      multiplet_handling = c("voidmultiplets", "takemaxarea"), ## Changed value here
                                      area_col = "AreaShape_Area") {

  # --- Input Validation ---
  if (!is.data.frame(df1)) stop("`df1` must be a data frame.")
  if (!is.data.frame(df2)) stop("`df2` must be a data frame.")

  if (!is.character(key1_col) || length(key1_col) != 1) {
    stop("`key1_col` must be a single character string.")
  }
  if (!key1_col %in% names(df1)) {
    stop("`key1_col` ('", key1_col, "') not found in `df1`.")
  }

  if (!is.character(key2_col) || length(key2_col) != 1) {
    stop("`key2_col` must be a single character string.")
  }
  if (!key2_col %in% names(df2)) {
    stop("`key2_col` ('", key2_col, "') not found in `df2`.")
  }

  # Match the multiplet handling argument
  multiplet_handling <- match.arg(multiplet_handling)

  if (multiplet_handling == "takemaxarea") { # Changed value check here
    if (!is.character(area_col) || length(area_col) != 1) {
      stop("`area_col` must be a single character string when using 'takemaxarea'.") # Changed value name in message
    }
    if (!area_col %in% names(df2)) {
      stop("`area_col` ('", area_col, "') not found in `df2`.")
    }
    if (!is.numeric(df2[[area_col]])) {
      warning("`area_col` ('", area_col, "') in `df2` is not numeric. Coercing to numeric.")
      # Use suppressWarnings to avoid messages if coercion introduces NAs intentionally
      suppressWarnings(df2[[area_col]] <- as.numeric(df2[[area_col]]))
      if(all(is.na(df2[[area_col]]))) {
        stop("Coercion of `area_col` ('", area_col, "') to numeric resulted in all NAs.")
      }
    }
  }

  # --- Prepare df2 based on multiplet handling strategy ---

  # Identify keys in df2 that appear more than once
  multiplet_keys_df2 <- df2 %>%
    dplyr::group_by(.data[[key2_col]]) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::distinct(.data[[key2_col]]) %>%
    dplyr::pull(.data[[key2_col]])

  df2_processed <- df2

  if (length(multiplet_keys_df2) > 0) {
    if (multiplet_handling == "voidmultiplets") {
      # For 'voidmultiplets', filter out *all* rows from df2 that have multiple entries for the key.
      # The left_join will then naturally introduce NAs for these keys from df1.
      df2_processed <- df2 %>%
        dplyr::filter(!(.data[[key2_col]] %in% multiplet_keys_df2))

    } else if (multiplet_handling == "takemaxarea") { # Changed value check here
      # For 'takemaxarea', process df2:
      # 1. Separate unique key rows and multiplet key rows
      # 2. For multiplets, keep only the row with max area per key
      # 3. Combine unique rows and processed multiplet rows

      df2_unique_keys <- df2 %>%
        dplyr::filter(!(.data[[key2_col]] %in% multiplet_keys_df2))

      df2_multiplets_processed <- df2 %>%
        dplyr::filter(.data[[key2_col]] %in% multiplet_keys_df2) %>%
        dplyr::group_by(.data[[key2_col]]) %>%
        # Keep only the row with the maximum area.
        # na.rm=TRUE handles potential NAs in area_col.
        # with_ties=FALSE ensures only one row is returned if areas tie.
        dplyr::slice_max(order_by = .data[[area_col]], n = 1, with_ties = FALSE, na_rm = TRUE) %>%
        dplyr::ungroup() # Ungroup after slicing

      # Combine the rows with unique keys and the selected rows from multiplets
      df2_processed <- dplyr::bind_rows(df2_unique_keys, df2_multiplets_processed)
    }
  }

  # --- Select columns to add (all except the key column from df2) ---
  cols_to_add <- setdiff(names(df2_processed), key2_col)
  if (length(cols_to_add) == 0) {
    warning("No columns to add from `df2` besides the key column ('", key2_col, "').")
    # Still return df1 to maintain behavior
    return(df1)
  }
  df2_final_for_join <- df2_processed %>%
    dplyr::select(dplyr::all_of(c(key2_col, cols_to_add)))

  # --- Perform the Left Join ---
  # Create the 'by' argument dynamically
  join_by_vec <- stats::setNames(key2_col, key1_col)

  merged_df <- dplyr::left_join(df1, df2_final_for_join, by = join_by_vec)

  # --- Rename added columns if they conflict with existing df1 columns ---
  # (excluding the key1_col which is handled by the join)
  original_df1_cols <- setdiff(names(df1), key1_col)
  # Identify columns present in the final merged_df that were NOT in the original df1
  added_cols <- setdiff(names(merged_df), names(df1))
  # Find the intersection between the newly added columns and the original df1 columns (excluding the key)
  cols_from_df2_that_conflict <- intersect(original_df1_cols, added_cols)

  if (length(cols_from_df2_that_conflict) > 0) {
    warning("Columns from `df2` (", paste(cols_from_df2_that_conflict, collapse=", "),
            ") conflict with existing columns in `df1`. Renaming added columns with suffix '.df2'.")

    # Create a function to rename only the conflicting columns added from df2
    rename_conflicts <- function(name) {
      if (name %in% cols_from_df2_that_conflict) {
        return(paste0(name, ".df2"))
      } else {
        return(name)
      }
    }
    # Apply renaming selectively only to the columns that were actually added
    merged_df <- merged_df %>%
      dplyr::rename_with(rename_conflicts, .cols = dplyr::any_of(added_cols))
  }


  return(merged_df)
}

###################################################################################################


#' 'addCellProfilerAttributesSCE()': Add CellProfiler Attributes to colData of Bioconductor Objects
#'
#' A wrapper function for `addCellProfilerAttributes` that operates on the
#' `colData` of `SummarizedExperiment`, `SingleCellExperiment`, or `QFeatures`
#' objects.
#'
#' @details
#' This function extracts the `colData` from the input object (`sce_obj`),
#' converts it to a standard `data.frame`, and passes it as `df1` to the
#' `addCellProfilerAttributes` function along with `df2` and other parameters.
#' The resulting merged data frame is then converted back to a `DataFrame` and
#' used to replace the original `colData` in the input object.
#'
#' The row names of the `colData` are preserved throughout the process.
#' For `QFeatures` objects, this function modifies the primary (top-level)
#' `colData`.
#'
#' See `?addCellProfilerAttributes` for details on the merging logic,
#' `multiplet_handling` options, and other parameters.
#'
#' @param sce_obj A `SummarizedExperiment`, `SingleCellExperiment`, or
#'   `QFeatures` object.
#' @param df2 The secondary data frame (data.frame or tibble) containing the
#'   attributes to add. Passed directly to `addCellProfilerAttributes`.
#' @param key1_col The name of the key column *in the `colData` of `sce_obj`*
#'   (character string). Defaults to `"Cropped_Path"`. Passed to
#'   `addCellProfilerAttributes`.
#' @param key2_col The name of the key column in `df2` (character string).
#'   Defaults to `"FileName_Image"`. Passed to `addCellProfilerAttributes`.
#' @param multiplet_handling Method to handle multiple rows for the same key in `df2`.
#'   Must be one of `"voidmultiplets"` or `"takemaxarea"` (character string).
#'   Defaults to `"voidmultiplets"`. Passed to `addCellProfilerAttributes`.
#' @param area_col The name of the column in `df2` containing the area values,
#'   required only if `multiplet_handling = "takemaxarea"` (character string).
#'   Defaults to `"AreaShape_Area"`. Passed to `addCellProfilerAttributes`.
#'
#' @return An object of the same class as `sce_obj` with its `colData` updated
#'   to include the merged attributes from `df2`.
#'
#' @examples
#'
#' # --- Create Sample SummarizedExperiment ---
#' library(SummarizedExperiment)
#' library(S4Vectors)
#' counts <- matrix(rpois(100, lambda = 10), ncol=10, nrow=10)
#' rownames(counts) <- paste0("Gene", 1:10)
#' colnames(counts) <- paste0("Cell", 1:10)
#'
#' sample_coldata <- DataFrame(
#'    SampleID = paste0("S", 1:10),
#'    Treatment = rep(c("A", "B"), 5),
#'    # Key column matching df1's perspective in addCellProfilerAttributes
#'    Cropped_Path = paste0("path/", letters[1:10]),
#'    row.names = colnames(counts) # Ensure row names match assay colnames
#' )
#'
#' sce <- SummarizedExperiment(assays=list(counts=counts),
#'                             colData=sample_coldata)
#'
#' print("Original colData:")
#' print(colData(sce))
#'
#' # --- Create Sample df2 (attributes to add) ---
#' df2_attribs <- data.frame(
#'   FileName_Image = c("path/a", "path/b", "path/b", "path/c", "path/f", "path/g"),
#'   QC_metric = rnorm(6, mean=100),
#'   AreaShape_Area = c(50, 60, 55, 70, 80, 90),
#'   Batch = rep(c("X", "Y"), 3)
#' )
#' print("Attributes to add (df2):")
#' print(df2_attribs)
#'
#' @export
#' @importFrom SummarizedExperiment colData colData<- SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @importFrom methods is
#'
addCellProfilerAttributesSCE <- function(sce_obj,
                                         df2,
                                         key1_col = "Cropped_Path",
                                         key2_col = "FileName_Image",
                                         multiplet_handling = c("voidmultiplets", "takemaxarea"),
                                         area_col = "AreaShape_Area") {

  # --- Input Validation ---
  # Check if sce_obj is one of the expected Bioconductor classes
  # QFeatures also inherits from SummarizedExperiment for primary colData access
  if (!methods::is(sce_obj, "SummarizedExperiment")) {
    stop("`sce_obj` must be a SummarizedExperiment, SingleCellExperiment, or QFeatures object.")
  }

  # Basic check for df2 (more thorough checks happen in addCellProfilerAttributes)
  if (!is.data.frame(df2)) stop("`df2` must be a data frame.")

  # Ensure multiplet_handling is valid before extracting colData
  # Use match.arg directly; addCellProfilerAttributes will do it again, but good practice here too.
  multiplet_handling <- match.arg(multiplet_handling)

  # --- Extract colData ---
  original_coldata <- SummarizedExperiment::colData(sce_obj)

  if (nrow(original_coldata) == 0) {
    warning("colData of `sce_obj` has 0 rows. Returning object unchanged.")
    return(sce_obj)
  }

  # Preserve row names and convert to data.frame for the merge function
  original_rownames <- rownames(original_coldata)
  df1 <- as.data.frame(original_coldata)

  # Ensure original row names were preserved if conversion put them in a column
  # (as.data.frame usually keeps them as row names for DataFrame)
  if (!identical(rownames(df1), original_rownames)) {
    # This case is less likely with DataFrame -> data.frame but good to check
    if("row.names" %in% names(df1) && identical(df1$row.names, original_rownames)) {
      rownames(df1) <- df1$row.names
      df1$row.names <- NULL # remove redundant column if created
    } else if (!is.null(original_rownames)) {
      # Fallback if conversion truly lost them
      warning("Row names may have been lost during colData extraction. Attempting to reassign.")
      if(nrow(df1) == length(original_rownames)) {
        rownames(df1) <- original_rownames
      } else {
        stop("Row name mismatch after colData extraction. Cannot proceed.")
      }
    }
  }


  # --- Call the core merging function ---
  merged_df <- addCellProfilerAttributes(
    df1 = df1,
    df2 = df2,
    key1_col = key1_col,
    key2_col = key2_col,
    multiplet_handling = multiplet_handling,
    area_col = area_col
  )

  # --- Validation after merge ---
  if (nrow(merged_df) != nrow(df1)) {
    stop("Merging resulted in an incorrect number of rows. Cannot update colData.")
  }

  # --- Prepare new colData and update object ---
  # Convert back to DataFrame, ensuring original row names are preserved/reinstated
  new_coldata <- S4Vectors::DataFrame(merged_df, row.names = original_rownames)

  # Replace the colData in the original object
  SummarizedExperiment::colData(sce_obj) <- new_coldata

  # --- Return updated object ---
  return(sce_obj)
}
