### Functions for analysing cellenONE image metadata produced by cellpose or CellProfiler

# Currently this includes
# 1. countCells()

##############################################################################################################

#' 'countCells()' Count Segmented Objects in Cellpose PNG Masks
#'
#' @description
#' This function processes Cellpose-generated PNG segmentation masks to count the
#' number of unique objects (e.g., cells, nuclei) identified in each mask.
#' It assumes masks use unique positive integer labels for objects and a
#' specified background value (defaulting to 0). It uses the 'magick' package
#' for image reading and processing.
#'
#' @param path A character string specifying the path to either a single PNG mask
#'   file or a directory containing multiple PNG mask files. Default is `NULL`.
#' @param files A character vector of full paths to specific PNG mask files. If
#'   provided, this overrides the `path` and `suffix` arguments. Default is `NULL`.
#' @param suffix A character string representing the filename suffix used to
#'   identify mask files when `path` points to a directory and `files` is `NULL`.
#'   Default is `"_masks.png"`. The pattern matching looks for files ending with
#'   this suffix.
#' @param background_value A single numeric value representing the pixel value
#'   assigned to the background in the masks. This value will be excluded from
#'   the object count. Default is `0`.
#'
#' @return A dataframe with two columns:
#'   \itemize{
#'     \item `FileName`: The base name of the processed PNG file.
#'     \item `CP_CellCount`: The integer count of unique objects (masks) found
#'       in the file, excluding the background value. Returns `NA` if the file
#'       could not be processed due to errors.
#'   }
#'
#' @importFrom magick image_read image_data image_info
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Process all files ending in "_seg.png" in a directory
#' # Assume "/path/to/masks" contains mask files like "image1_seg.png", "image2_seg.png"
#' results_dir <- countCells(path = "/path/to/masks", suffix = "_seg.png")
#' print(results_dir)
#'
#' # Example 2: Process a specific list of files
#' file_list <- c("/path/to/masks/image1_seg.png", "/path/to/masks/image3_seg.png")
#' results_list <- countCells(files = file_list)
#' print(results_list)
#'
#' # Example 3: Process a single file specified by path
#' results_single <- countCells(path = "/path/to/masks/image1_seg.png")
#' print(results_single)
#'
#' # Example 4: Process files where background pixel value is 255
#' results_bg <- countCells(path = "/path/to/masks_alt_bg", suffix = ".png", background_value = 255)
#' print(results_bg)
#'
#' # Example demonstrating NA for a non-existent or corrupt file
#' # Create a dummy file list including a non-existent one
#' # (Assuming 'real_mask.png' exists and 'fake_mask.png' does not)
#' mixed_files <- c("real_mask.png", "fake_mask.png")
#' # Need to ensure 'real_mask.png' is a valid PNG mask for a meaningful result
#' # results_mixed <- countCells(files = mixed_files)
#' # print(results_mixed) # Would show count for real_mask.png and NA for fake_mask.png
#' }
#'
countCells <- function(path = NULL, files = NULL, suffix = "_masks.png", background_value = 0) {

  # --- Input Validation ---
  if (is.null(files) && is.null(path)) {
    stop("Either 'path' or 'files' must be provided.")
  }
  if (!is.null(files) &&!is.character(files)) {
    stop("'files' must be a character vector of file paths.")
  }
  if (!is.null(path) &&!is.character(path) && length(path)!= 1) {
    stop("'path' must be a single character string.")
  }
  if (!is.numeric(background_value) || length(background_value)!= 1) {
    stop("'background_value' must be a single numeric value.")
  }

  # --- Identify Target Files ---
  target_files <- character()
  if (!is.null(files)) {
    target_files <- files
  } else if (!is.null(path)) {
    if (dir.exists(path)) {
      target_files <- list.files(path = path,
                                 pattern = paste0(utils::glob2rx(paste0("*", suffix))), # Use glob2rx for more robust pattern matching
                                 full.names = TRUE,
                                 recursive = FALSE)
      if (length(target_files) == 0) {
        warning("No files found in directory '", path, "' with suffix '", suffix, "'.")
        # Return empty dataframe matching the expected structure
        return(data.frame(FileName = character(0), CP_CellCount = integer(0)))
      }
    } else if (file.exists(path)) {
      target_files <- path
    } else {
      stop("Provided 'path' does not exist or is not a valid directory or file: ", path)
    }
  }

  if (length(target_files) == 0) {
    warning("No valid target files were identified.")
    return(data.frame(FileName = character(0), CP_CellCount = integer(0)))
  }

  # --- Process Files ---
  results_list <- vector("list", length(target_files))
  names(results_list) <- basename(target_files)

  for (i in seq_along(target_files)) {
    file_path <- target_files[i]
    file_base <- basename(file_path)
    cell_count <- NA_integer_ # Default to NA in case of error

    # Check file existence again (robustness)
    if (!file.exists(file_path)) {
      warning("File not found during processing: ", file_path)
      results_list[[i]] <- data.frame(FileName = file_base, CP_CellCount = cell_count, stringsAsFactors = FALSE)
      next # Skip to next file
    }

    # Check if it's likely a PNG before trying to read fully
    tryCatch({
      info <- magick::image_info(magick::image_read(file_path, depth = 16)) # Try reading with depth hint
      if (toupper(info$format)!= "PNG") {
        warning("Skipping non-PNG file: ", file_path, " (Format: ", info$format, ")")
        results_list[[i]] <- data.frame(FileName = file_base, CP_CellCount = cell_count, stringsAsFactors = FALSE)
        next # Skip to next file
      }
    }, error = function(e_info){
      # If image_info itself fails, likely can't read the file
      warning("Failed to get image info (file might be corrupt or unreadable): ", file_path, " - Error: ", conditionMessage(e_info))
      # Keep cell_count as NA, proceed to store result
    })

    # If info check passed or failed gracefully, try full processing
    if (!is.na(cell_count)) { # Only proceed if not already skipped or failed at info stage
      tryCatch({
        # Read the image
        img <- magick::image_read(file_path, depth = 16) # Hint depth again

        # Extract pixel data - request grayscale
        # Note: This assumes Cellpose saves masks as grayscale PNGs.
        # The result is typically a raw array (channels x height x width)
        pixels_raw <- magick::image_data(img, channels = 'gray')

        # Convert raw data to integer vector
        # CRITICAL ASSUMPTION: Assumes direct conversion works for the PNG bit depth.
        # May fail or misinterpret for > 8-bit grayscale PNGs if magick doesn't handle it transparently.
        pixels_int <- as.integer(pixels_raw)

        # Get unique pixel values
        unique_values <- unique(pixels_int)

        # Filter out the background value
        mask_ids <- unique_values[unique_values!= background_value]

        # Count the remaining unique values (these are the object IDs)
        cell_count <- length(mask_ids)

      }, error = function(e_proc) {
        # Handle errors during image reading or processing
        warning("Failed to process image ", file_path, ": ", conditionMessage(e_proc))
        # cell_count remains NA
      })
    }

    # Store result for this file
    results_list[[i]] <- data.frame(FileName = file_base, CP_CellCount = cell_count, stringsAsFactors = FALSE)
  }

  # --- Combine results into a single dataframe ---
  final_df <- do.call(rbind, results_list)
  rownames(final_df) <- NULL # Remove row names derived from the list

  return(final_df)
}

##############################################################################################################

# Function: add number of cell metadata columns based on  cellpose mask files to QFeatures object

# Function: integrate CellProfiler output into annotation dataframe
# Function: integrate CellProfiler output tables into QFeatures object

