### Functions for processing cellenONE images, and overlaying masks produced by cellpose onto these images

# Currently this includes:
# 1. cropImages() - Status: functional
# 2. npy_masks_to_png() - Status: functional (some maxColorValue errors (3% of files)) -
# - confirmed, maxColorValue error means that there is no mask/cell identified in that file. Script good.
# 3. overlayMask() - Status: functional
# 4. overlayMaskOnParent() - Status: testing

##############################################################################################################

#' cropImages: Generate Cropped Images from Image Paths and Coordinates
#'
#' Reads images specified in a data frame, crops them based on a user-defined
#' offset centred on user-provided pixel coordinates, and saves the cropped images to a specified
#' output directory. Allows specifying a base input directory if image paths
#' in the data frame are relative filenames. Output filenames are derived from the
#' input `ImageFile` names, with options to add a prefix and replace the file extension
#' with a custom suffix. For the intended use case, the dataframe
#' represents the cellenONE cell sorting output file containing cell metadata and image names,
#' the images are the images recorded for each sorted cell, and the XY coordinates represent
#' the location of the cell in the image. The function crops a square region surrounding the cell
#' of interest, with the size of the square determined by the pixel offset.
#'
#' @param df A data frame containing image processing information. **Required.**
#'   Must include columns: `ImageFile` (character, full path to the input image
#'   or filename relative to `input_dir` if provided),
#'   `X` (numeric, x-coordinate of the center point for cropping),
#'   `Y` (numeric, y-coordinate of the center point for cropping).
#'   The `output_filename` column is no longer required.
#' @param pixel_offset An integer specifying the number of pixels to offset from
#'   the center coordinates (`X`, `Y`) in each direction (up, down,
#'   left, right) to define the cropping box. **Required.** The resulting crop
#'   dimensions will be (2 * $pixel_offset$) x (2 * $pixel_offset$). Must be a
#'   positive integer.
#' @param output_dir A character string specifying the path to the directory
#'   where cropped images should be saved. **Required.**
#' @param input_dir An optional character string specifying a base directory
#'   for input images. If provided, paths in `df$ImageFile` are treated as
#'   relative to this directory (e.g., if `input_dir` is "/path/to/images" and
#'   `df$ImageFile` contains "img1.png", the function will look for
#'   "/path/to/images/img1.png"). If `NULL` (the default), `df$ImageFile`
#'   must contain full paths or paths relative to the current working directory.
#' @param output_prefix A character string to prepend to the derived output filenames.
#'   Defaults to `"cropped_"'. Optional.
#' @param output_suffix An optional character string used to replace the original file
#'   extension in the output filename. For example, if `output_suffix` is `"_cropped.png"`,
#'   an input file `image1.tif` will result in an output file named `image1_cropped.png`
#'   (plus any `output_prefix`). If the original filename has no extension, the suffix
#'   is simply appended. If `NULL` or `""` (the default), the original filename
#'   (including its extension) from `ImageFile` is used as the base for the output filename.
#' @param create_dir A logical value indicating whether the `output_dir` should
#'   be created if it does not exist. Defaults to `FALSE`. If `TRUE`, the
#'   function will attempt to create the directory recursively. If `FALSE` and
#'   the directory does not exist, the function will stop with an error. Optional.
#'
#' @return Invisibly returns `NULL`. The function's primary effect is saving
#'   cropped image files to the specified `output_dir`.
#' @export
#' @importFrom magick image_read image_crop image_write image_info

cropImages <- function(df, pixel_offset, output_dir, input_dir = NULL, output_prefix = "cropped_", output_suffix = NULL, create_dir = FALSE) {

  # --- Input Validation ---
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }
  # Updated required column names (removed 'output_filename')
  required_cols <- c("ImageFile", "X", "Y")
  if (!all(required_cols %in% names(df))) {
    # Updated error message to reflect new column names
    stop("`df` must contain columns: ", paste(required_cols, collapse = ", "))
  }
  if (!is.numeric(pixel_offset) || length(pixel_offset)!= 1 || pixel_offset <= 0 || floor(pixel_offset)!= pixel_offset) {
    stop("`pixel_offset` must be a single positive integer.")
  }
  if (!is.character(output_dir) || length(output_dir)!= 1 || nchar(output_dir) == 0) {
    stop("`output_dir` must be a non-empty character string.")
  }
  if (!is.null(input_dir)) {
    if (!is.character(input_dir) || length(input_dir)!= 1 || nchar(input_dir) == 0) {
      stop("`input_dir`, if provided, must be a non-empty character string.")
    }
    if (!dir.exists(input_dir)) {
      stop("Specified `input_dir` does not exist: ", input_dir)
    }
  }
  if (!is.character(output_prefix) || length(output_prefix)!= 1) {
    stop("`output_prefix` must be a single character string.")
  }
  # Validation for the new output_suffix parameter
  if (!is.null(output_suffix) && (!is.character(output_suffix) || length(output_suffix)!= 1)) {
    stop("`output_suffix`, if provided, must be a single character string.")
  }
  if (!is.logical(create_dir) || length(create_dir)!= 1 || is.na(create_dir)) {
    stop("`create_dir` must be a single logical value (TRUE or FALSE).")
  }

  # Output Directory existence check / creation
  if (!dir.exists(output_dir)) {
    if (create_dir) {
      message("Output directory '", output_dir, "' does not exist. Attempting to create.")
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      # Verify creation
      if (!dir.exists(output_dir)) {
        stop("Failed to create output directory: '", output_dir, "'. Please check path and permissions.")
      }
    } else {
      stop("Output directory '", output_dir, "' does not exist. Set `create_dir = TRUE` to attempt creation.")
    }
  }

  # Check if magick is installed
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The 'magick' package is required but not installed. Please install it via install.packages('magick').")
  }

  # --- Image Processing Loop ---
  for (i in 1:nrow(df)) {
    # Determine the full input image path using the 'ImageFile' column
    relative_image_path <- df$ImageFile[i]
    if (!is.null(input_dir)) {
      current_image_path <- file.path(input_dir, relative_image_path)
    } else {
      current_image_path <- relative_image_path
    }

    # Extract coordinates using the 'X' and 'Y' columns
    x_coord <- df$X[i]
    y_coord <- df$Y[i]
    # output_filename is now derived, not read from df

    # Validate row-specific inputs (using relative_image_path for clarity)
    if (is.na(relative_image_path) ||!is.character(relative_image_path) || nchar(relative_image_path) == 0) {
      warning("Row ", i, ": Invalid or missing `ImageFile` value. Skipping.")
      next
    }
    if (is.na(x_coord) ||!is.numeric(x_coord) || is.na(y_coord) ||!is.numeric(y_coord)) {
      warning("Row ", i, ": Invalid or missing coordinates (`X`, `Y`) for image '", relative_image_path, "'. Skipping.")
      next
    }
    # Removed validation for output_filename column

    # Check if constructed input image file exists
    if (!file.exists(current_image_path)) {
      warning("Row ", i, ": Image file not found at '", current_image_path, "'. Skipping.")
      next
    }

    tryCatch({
      # --- Derive Output Filename ---
      base_input_filename <- basename(relative_image_path)
      output_filename <- "" # Initialize

      if (!is.null(output_suffix) && nzchar(output_suffix)) {
        # If suffix is provided, remove original extension and add suffix
        filename_no_ext <- sub("(\\.[^.]+)$", "", base_input_filename)
        output_filename <- paste0(filename_no_ext, output_suffix)
      } else {
        # Otherwise, use the original base filename
        output_filename <- base_input_filename
      }

      # --- Image Processing ---
      # Read the image
      img <- magick::image_read(current_image_path)
      img_info <- magick::image_info(img)

      # Calculate crop geometry (adjusting for image boundaries)
      img_width <- img_info$width
      img_height <- img_info$height

      # Define crop box corners based on center and offset
      left <- max(0, round(x_coord - pixel_offset))
      top <- max(0, round(y_coord - pixel_offset))
      right <- min(img_width, round(x_coord + pixel_offset))
      bottom <- min(img_height, round(y_coord + pixel_offset))

      # Calculate width and height for magick::image_crop
      crop_width <- right - left
      crop_height <- bottom - top

      if (crop_width <= 0 || crop_height <= 0) {
        warning("Row ", i, ": Calculated crop dimensions are non-positive (width=", crop_width, ", height=", crop_height, ") for image '", current_image_path, "'. Skipping.")
        next
      }

      # Format for magick::image_crop: "widthxheight+xoffset+yoffset"
      geometry <- paste0(crop_width, "x", crop_height, "+", left, "+", top)

      # Crop the image
      cropped_img <- magick::image_crop(img, geometry)

      # Construct output path using the derived output_filename and prefix
      output_path <- file.path(output_dir, paste0(output_prefix, output_filename))

      # Write the cropped image
      magick::image_write(cropped_img, path = output_path)

    }, error = function(e) {
      warning("Row ", i, ": Failed to process image '", current_image_path, "'. Error: ", e$message)
    }) # End tryCatch
  } # End loop

  invisible(NULL)
}

##############################################################################################################
#' 'npy_masks_to_png()' converts Cellpose.npy Masks to PNG Images
#'
#' Reads.npy segmentation files #' extracts the mask data, generates a pseudo-colored representation,
#' and saves the masks as PNG images using the magick package.
#'
#' @param input_dir Character string. Path to the directory containing.npy files.
#' @param output_dir Optional character string. Path to the directory where PNG files
#'   will be saved. If NULL (default), PNG files are saved in `input_dir`.
#'   The directory will be created if it doesn't exist.
#' @param input_suffix Optional character string. A regular expression specifying the
#'   ending pattern of input files to process. Defaults to "_seg\\\\.npy$".
#' @param output_suffix Optional character string. Suffix to use for the output PNG
#'   files (including the '.png' extension). Defaults to "_cp_masks.png".
#'
#' @return Invisibly returns a list containing vectors of successfully processed
#'   input file paths (`success`) and a list of errors encountered (`errors`),
#'   where names are input file paths and values are error messages. Primarily
#'   called for its side effect of writing PNG files.
#'
#' @details This function requires the 'reticulate' and 'magick' packages.
#'   It also depends on a Python environment accessible by 'reticulate'
#'   with the 'numpy' library installed. The function assumes the.npy files
#'   contain pickled Python objects loadable with `numpy.load(..., allow_pickle=TRUE)`
#'   and that the loaded object is a dictionary (or similar structure)
#'   containing an element named 'masks' which holds the integer segmentation mask array.
#'   A unique color (plus black for background ID 0) is assigned to each segment ID
#'   found in the mask array using `grDevices::rainbow()`.
#'
#' @export
#' @importFrom reticulate import py_available py_module_available
#' @importFrom magick image_read image_write
#' @importFrom tools file_path_sans_ext
#' @importFrom grDevices as.raster rainbow
#'
#' @examples
#' \dontshow{if (reticulate::py_available(initialize = TRUE) && reticulate::py_module_available("numpy")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
#' # Setup: Create temporary directories and a dummy.npy file
#' temp_in_dir <- tempfile("input_dir_")
#' temp_out_dir <- tempfile("output_dir_")
#' dir.create(temp_in_dir)
#' dir.create(temp_out_dir)
#'
#' # Create a dummy mask array (e.g., 3x4 with segments 1, 2, 3)
#' dummy_masks <- array(as.integer(c(0,1,1,0, 2,2,0,2, 0,3,3,0)), dim = c(3, 4))
#' # Create the dictionary structure expected within the.npy
#' dummy_data_to_save <- reticulate::dict("masks" = dummy_masks)
#'
#' # Save the dummy data to a.npy file using numpy via reticulate
#' np <- reticulate::import("numpy")
#' npy_path <- file.path(temp_in_dir, "example_seg.npy")
#' tryCatch({
#'   np$save(npy_path, dummy_data_to_save)
#'
#'   # Run the conversion function
#'   results <- npy_masks_to_png(input_dir = temp_in_dir, output_dir = temp_out_dir)
#'
#'   # Check results (output file should exist)
#'   print(list.files(temp_out_dir))
#'   print(results)
#'
#' }, error = function(e) {
#'   message("Example skipped: numpy unavailable or failed to save file.")
#' })
#'
#' # Cleanup
#' unlink(temp_in_dir, recursive = TRUE, force = TRUE)
#' unlink(temp_out_dir, recursive = TRUE, force = TRUE)
#' \dontshow{\}) # examplesIf}
npy_masks_to_png <- function(input_dir,
                             output_dir = NULL,
                             input_suffix = "_seg\\.npy$",
                             output_suffix = "_cp_masks.png") {

  # --- Parameter Validation ---
  if (!is.character(input_dir) || length(input_dir)!= 1 ||!dir.exists(input_dir)) {
    stop("'input_dir' must be an existing directory path.")
  }
  if (is.null(output_dir)) {
    output_dir <- input_dir
  } else {
    if (!is.character(output_dir) || length(output_dir)!= 1) {
      stop("'output_dir' must be NULL or a single character string path.")
    }
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(output_dir)) {
        stop("Failed to create 'output_dir': ", output_dir)
      }
    }
  }
  if (!is.character(input_suffix) || length(input_suffix)!= 1) {
    stop("'input_suffix' must be a single character string (regex).")
  }
  if (!is.character(output_suffix) || length(output_suffix)!= 1) {
    stop("'output_suffix' must be a single character string.")
  }

  # --- Dependency Check ---
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("Python is not available. Please configure 'reticulate'.")
  }
  if (!reticulate::py_module_available("numpy")) {
    stop("Python module 'numpy' not found. Please install numpy (e.g., reticulate::py_install('numpy')).")
  }

  # --- File Discovery ---
  npy_files <- list.files(path = input_dir,
                          pattern = input_suffix,
                          full.names = TRUE,
                          ignore.case = TRUE, # Be flexible with case
                          recursive = FALSE) # Process only top-level files

  if (length(npy_files) == 0) {
    message("No files found matching the pattern '", input_suffix, "' in directory '", input_dir, "'.")
    return(invisible(list(success = character(0), errors = list())))
  }

  # --- Initialization for Processing ---
  results <- list(success = character(0), errors = list())
  np <- reticulate::import("numpy", convert = TRUE) # Import once

  # --- Core Processing Loop ---
  for (npy_file in npy_files) {
    tryCatch({
      # 1. Construct Output Path
      base_name_no_ext <- tools::file_path_sans_ext(basename(npy_file))
      # Remove the input suffix part from the base name
      base_name_clean <- sub(input_suffix, "", basename(npy_file), ignore.case = TRUE)
      # Ensure the extension was also removed if suffix wasn't just extension
      base_name_clean <- tools::file_path_sans_ext(base_name_clean)

      output_base_name <- paste0(base_name_clean, output_suffix)
      output_png_path <- file.path(output_dir, output_base_name)

      # 2. Load.npy using reticulate
      loaded_data_py <- np$load(npy_file, allow_pickle = TRUE)

      # 3. Extract Masks (assuming dict structure)
      #if (!("masks" %in% names(loaded_data_py))) {
      #  stop("'.npy' file does not contain a 'masks' element.")
      #}
      # 3. Extract Masks (accessing the inner list)
      # First, check if the outer list structure is as expected
      if (length(loaded_data_py) < 1 ||!is.list(loaded_data_py[[1]])) {
        stop("'.npy' file structure is not the expected list containing an inner list.")
      }
      # Now, check if the inner list contains 'masks'
      inner_list <- loaded_data_py[[1]]
      if (!("masks" %in% names(inner_list))) {
        stop("Inner list of '.npy' file does not contain a 'masks' element.")
      }
      # Access the masks from the inner list
      masks_matrix <- inner_list$masks

      #masks_matrix <- loaded_data_py[[1]]$masks

      # Ensure it's a matrix or array
      if (!is.matrix(masks_matrix) &&!is.array(masks_matrix)) {
        stop("'masks' element is not a matrix or array.")
      }
      # Ensure integer type (or coerce) - numpy might return float
      if(!is.integer(masks_matrix)) {
        masks_matrix <- apply(masks_matrix, 1:2, as.integer)
      }


      # 4. Prepare for magick: Generate Color Map
      max_segment_id <- max(masks_matrix, 0)
      if (max_segment_id > 0) {
        # Check for excessive segments to avoid huge color maps / memory issues
        if (max_segment_id > 65534) { # uint16 max - 1 reasonable limit?
          warning("File '", basename(npy_file), "' has over 65534 segments. ",
                  "Color map generation might be slow or fail.", immediate. = TRUE)
        }
        segment_colors <- grDevices::rainbow(max_segment_id)
      } else {
        segment_colors <- character(0)
      }
      # Map 0->black, 1->col[1], 2->col...[2]
      full_color_map <- c("black", segment_colors)

      # 5. Convert to Raster
      # Using max argument to potentially help as.raster map values correctly
      raster_obj <- grDevices::as.raster(masks_matrix, col = full_color_map, max = max_segment_id)

      # 6. Read Raster and Write PNG using magick
      magick_image <- magick::image_read(raster_obj)
      magick::image_write(magick_image, path = output_png_path, format = "png")

      # 7. Record Success
      results$success <- c(results$success, npy_file)

    }, error = function(e) {
      # Log the error associated with the file
      error_message <- paste("Error processing file '", basename(npy_file), "': ", e$message, sep = "")
      # Use file path as name for the error list element
      results$errors[[npy_file]] <- error_message
      # Optionally issue an immediate warning per file if desired, but spec asked against it
      warning(error_message, immediate. = TRUE)
    }) # End tryCatch
  } # End for loop

  # --- Summary Reporting ---
  num_success <- length(results$success)
  num_errors <- length(results$errors)

  if (num_errors > 0) {
    # Combine error messages for summary warning
    error_summary <- paste(sapply(results$errors, function(msg) paste("-", msg)), collapse = "\n")
    warning(sprintf("Finished processing. Successfully converted %d files, but encountered errors with %d files:\n%s",
                    num_success, num_errors, error_summary))
  } else if (num_success > 0) {
    message(sprintf("Successfully converted %d files from '%s' to '%s'.",
                    num_success, input_dir, output_dir))
  } else if (length(npy_files) > 0) {
    # Files were found, but all failed
    warning("Finished processing, but failed to convert any of the ", length(npy_files), " matching files found.")
  }
  # No message needed if no files were found initially (handled earlier)

  # Return results invisibly
  invisible(results)
}

##############################################################################################################

#' Overlay Segmentation Mask on Image (Single File or Directory)
#'
#' @description Reads a microscopy image and a corresponding segmentation mask
#' (or directories containing multiple images/masks), then creates an output
#' image (or images) showing the mask as either outlines or a semi-transparent
#' overlay on the original image. Uses the 'magick' package. Console output
#' is minimized, primarily showing warnings for missing files or processing errors.
#'
#' @param image_input Path to the original microscopy image file OR a directory
#' containing multiple image files.
#' @param mask_input Path to the segmentation mask image file OR a directory
#' containing multiple mask files. If `image_input` is a directory, `mask_input`
#' can be the same directory or a separate one.
#' @param output_target Path to save the resulting composite image file OR a
#' directory where multiple output images will be saved. If inputs are
#' directories, this must be a directory path.
#' @param mode Character string: "outline" or "overlay". Determines the
#' visualization style. Default is "outline".
#' @param image_suffix Character string or NULL. Suffix used to identify image
#' files when `image_input` is a directory. Required if `image_input` and
#' `mask_input` point to the same directory. Example: `"_Run.png"`. If NULL
#' and separate directories are used, attempts to match common image extensions.
#' Default is NULL.
#' @param mask_suffix Character string. Suffix used to identify mask files when
#' `mask_input` (or `image_input`) is a directory. Default is `"_cp_masks.png"`.
#' @param output_suffix Character string. Suffix used for the generated output
#' files. The mask file's `mask_suffix` will be replaced with this.
#' Default is `"_cp_masks_overlay.png"`.
#' @param outline_col Color for the outlines (used if mode="outline"). Can be any
#' format recognized by `magick`. Default is "cyan".
#' @param outline_lwd Approximate line width (thickness) for outlines in pixels
#' (used if mode="outline"). Default is 2.
#' @param overlay_col Color for the overlay (used if mode="overlay"). Can be any
#' format recognized by `magick`. Default is "yellow".
#' @param overlay_alpha Alpha transparency for the overlay (used if mode="overlay").
#' A numeric value between 0 (fully transparent) and 1 (fully opaque). Default is 0.4.
#'
#' @return Invisibly returns a list of paths to the successfully created output files.
#' @export
#' @importFrom magick image_read image_info image_convert image_edge image_transparent image_blank image_composite image_write image_morphology image_fx
#' @importFrom grDevices col2rgb dev.off graphics.off
#' @importFrom graphics symbols
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' \dontrun{
#' # --- Setup Dummy Files/Dirs ---
#' if (requireNamespace("magick", quietly = TRUE)) {
#' # Create temp directories
#' temp_dir <- tempdir()
#' img_dir <- file.path(temp_dir, "images")
#' mask_dir <- file.path(temp_dir, "masks")
#' out_dir <- file.path(temp_dir, "output")
#' dir.create(img_dir, showWarnings = FALSE)
#' dir.create(mask_dir, showWarnings = FALSE)
#' dir.create(out_dir, showWarnings = FALSE)
#'
#' # Create dummy image/mask files (1 pair)
#' img_base1 <- "cell_image_A1"
#' img_suffix <- "_microscopy.png"
#' mask_suffix <- "_seg_mask.png"
#' img_path1 <- file.path(img_dir, paste0(img_base1, img_suffix))
#' mask_path1 <- file.path(mask_dir, paste0(img_base1, mask_suffix))
#'
#' img <- magick::image_blank(width=50, height=50, color="grey")
#' mask <- magick::image_blank(width=50, height=50, color="black")
#' mask <- magick::image_draw(mask)
#' graphics::symbols(25, 25, circles=10, inches=FALSE, add=TRUE, fg="white", bg="white")
#' grDevices::dev.off()
#' magick::image_write(img, img_path1)
#' magick::image_write(mask, mask_path1)
#'
#' # Create an image file without a corresponding mask
#' img_base_unmatched <- "unmatched_image"
#' img_path_unmatched <- file.path(img_dir, paste0(img_base_unmatched, img_suffix))
#' magick::image_write(img, img_path_unmatched)
#'
#' # Create a mask file without a corresponding image
#' mask_base_unmatched <- "unmatched_mask"
#' mask_path_unmatched <- file.path(mask_dir, paste0(mask_base_unmatched, mask_suffix))
#' magick::image_write(mask, mask_path_unmatched)
#'
#' # --- Example 1: Single File Mode (should be quiet) ---
#' out_single_file <- file.path(out_dir, "single_overlay.png")
#' overlayMask(img_path1, mask_path1, out_single_file, mode = "overlay")
#'
#' # --- Example 2: Directory Mode (should warn about unmatched files) ---
#' overlayMask(image_input = img_dir,
#' mask_input = mask_dir,
#' output_target = out_dir,
#' mode = "outline",
#' image_suffix = img_suffix, # Specify suffixes
#' mask_suffix = mask_suffix,
#' output_suffix = "_outline_overlay.png",
#' outline_col = "red")
#'
#' # --- Clean up ---
#' unlink(img_dir, recursive = TRUE)
#' unlink(mask_dir, recursive = TRUE)
#' unlink(out_dir, recursive = TRUE)
#' } else {
#' message("Magick package not installed. Examples cannot run.")
#' }
#' }
overlayMask <- function(image_input,
                         mask_input,
                         output_target,
                         mode = c("outline", "overlay"),
                         image_suffix = NULL,
                         mask_suffix = "_cp_masks.png",
                         output_suffix = "_cp_masks_overlay.png",
                         outline_col = "cyan",
                         outline_lwd = 2,
                         overlay_col = "yellow",
                         overlay_alpha = 0.4) {

  # --- Argument Validation ---
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The 'magick' package is required but not installed.")
  }
  mode <- match.arg(mode)
  is_valid_color <- function(color_string) {
    tryCatch({ grDevices::col2rgb(color_string); TRUE }, error = function(e) FALSE)
  }
  if (!is_valid_color(outline_col)) stop("Invalid color specified for outline_col: ", outline_col)
  if (!is_valid_color(overlay_col)) stop("Invalid color specified for overlay_col: ", overlay_col)
  if (!is.numeric(outline_lwd) || length(outline_lwd)!= 1 || outline_lwd <= 0) {
    stop("outline_lwd must be a single positive number.")
  }
  if (!is.numeric(overlay_alpha) || length(overlay_alpha)!= 1 || overlay_alpha < 0 || overlay_alpha > 1) {
    stop("overlay_alpha must be a single number between 0 and 1.")
  }
  if (!is.null(image_suffix) && (!is.character(image_suffix) || length(image_suffix) != 1)) {
    stop("image_suffix must be NULL or a single character string.")
  }
  if (!is.character(mask_suffix) || length(mask_suffix) != 1) {
    stop("mask_suffix must be a single character string.")
  }
  if (!is.character(output_suffix) || length(output_suffix) != 1) {
    stop("output_suffix must be a single character string.")
  }


  # --- Internal Processing Function for a Single Pair ---
  process_single_pair <- function(img_file, mask_file, out_file, mode,
                                  outline_col, outline_lwd, overlay_col, overlay_alpha) {
    original_img <- NULL
    mask_img <- NULL
    final_image <- NULL
    tryCatch({
      original_img <- magick::image_read(img_file)
      mask_img <- magick::image_read(mask_file)
      info_orig <- magick::image_info(original_img)[1, ]
      info_mask <- magick::image_info(mask_img)[1, ]
      if (info_orig$width != info_mask$width || info_orig$height != info_mask$height) {
        warning(paste("Skipping pair due to dimension mismatch:", basename(img_file), basename(mask_file)), call. = FALSE)
        return(NULL)
      }
      if (mode == "outline") {
        mask_gray <- magick::image_convert(mask_img, type = 'grayscale')
        edges <- magick::image_edge(mask_gray, radius = 1)
        kernel_size <- max(1, round(outline_lwd / 2))
        edges_thick <- magick::image_morphology(edges, method = 'Dilate', kernel = paste0('Disk:', kernel_size))
        color_layer <- magick::image_blank(width = info_orig$width, height = info_orig$height, color = outline_col)
        edges_thick_mask <- magick::image_convert(edges_thick, type = 'grayscale')
        colored_outlines_with_alpha <- magick::image_composite(
          image = color_layer,
          composite_image = edges_thick_mask,
          operator = "CopyOpacity"
        )
        final_image <- magick::image_composite(
          image = original_img,
          composite_image = colored_outlines_with_alpha,
          operator = "Over"
        )
      } else { # mode == "overlay"
        mask_transparent <- magick::image_transparent(mask_img, color = "black", fuzz = 10)
        rgb_vals <- grDevices::col2rgb(overlay_col)
        semi_transparent_color_string <- sprintf("rgba(%d, %d, %d, %f)",
                                                 rgb_vals[1], rgb_vals[2], rgb_vals[3],
                                                 overlay_alpha)
        semi_transparent_color_layer <- magick::image_blank(width = info_orig$width,
                                                            height = info_orig$height,
                                                            color = semi_transparent_color_string)
        overlay_layer_final <- magick::image_composite(
          image = semi_transparent_color_layer,
          composite_image = mask_transparent,
          operator = "In"
        )
        final_image <- magick::image_composite(
          image = original_img,
          composite_image = overlay_layer_final,
          operator = "Over"
        )
      }
      magick::image_write(final_image, path = out_file, format = "png")
      return(out_file)
    }, error = function(e) {
      warning(paste("Failed to process pair:", basename(img_file), "+", basename(mask_file), "->", basename(out_file), "\n Error:", e$message), call. = FALSE)
      return(NULL)
    })
  }


  # --- Determine Mode: Single File vs. Directory ---
  is_image_dir <- dir.exists(image_input)
  is_mask_dir <- dir.exists(mask_input)
  is_output_dir_intent <- dir.exists(output_target) || (!file.exists(output_target) && !grepl("\\.[^/\\.]+$", basename(output_target)))

  output_paths <- list()

  # --- Branch Logic ---

  if (!is_image_dir && !is_mask_dir) {
    # --- Single File Mode ---
    if (is_output_dir_intent) {
      if (!dir.exists(output_target)) {
        dir.create(output_target, recursive = TRUE, showWarnings = FALSE)
      }
      mask_basename <- basename(mask_input)
      safe_mask_suffix <- gsub("([\\.\\^\\$\\*\\+\\?\\(\\)\\{\\}\\|\\[\\]])", "\\\\\\1", mask_suffix)
      output_basename <- sub(pattern = paste0(safe_mask_suffix, "$"),
                             replacement = output_suffix,
                             x = mask_basename)
      if (output_basename == mask_basename) {
        warning(paste0("Mask suffix '", mask_suffix, "' not found in mask filename '", mask_basename, "'. Appending output suffix instead."), call. = FALSE)
        output_basename <- paste0(tools::file_path_sans_ext(mask_basename), output_suffix)
      }
      output_file_path <- file.path(output_target, output_basename)
    } else {
      output_file_path <- output_target
      out_dir <- dirname(output_file_path)
      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      }
    }
    if (!file.exists(image_input)) stop("Input image file not found: ", image_input)
    if (!file.exists(mask_input)) stop("Input mask file not found: ", mask_input)

    # Process the single pair (no message here)
    created_file <- process_single_pair(image_input, mask_input, output_file_path, mode, outline_col, outline_lwd, overlay_col, overlay_alpha)
    if (!is.null(created_file)) {
      output_paths[[1]] <- created_file
    }

  } else {
    # --- Directory Mode ---
    if (!is_output_dir_intent) {
      stop("In directory mode, output_target must be an existing or intended directory path.")
    }
    if (!dir.exists(output_target)) {
      dir.create(output_target, recursive = TRUE, showWarnings = FALSE)
    }

    image_dir <- NULL
    mask_dir <- NULL
    shared_directory <- FALSE

    if (is_image_dir && is_mask_dir && normalizePath(image_input) != normalizePath(mask_input)) {
      image_dir <- normalizePath(image_input)
      mask_dir <- normalizePath(mask_input)
      # message("Processing separate directories:\n Images: ", image_dir, "\n Masks: ", mask_dir) # Removed
    } else if (is_image_dir) {
      image_dir <- normalizePath(image_input)
      mask_dir <- image_dir
      shared_directory <- TRUE
      # message("Processing shared directory: ", image_dir) # Removed
      if (is.null(image_suffix)) {
        stop("image_suffix must be provided when using a single shared directory for images and masks.")
      }
      if (image_suffix == mask_suffix) {
        stop("image_suffix and mask_suffix must be different when using a single shared directory.")
      }
    } else {
      stop("Invalid combination of directory/file inputs. If using directory mode, image_input must be a directory.")
    }

    # List files based on suffixes
    safe_mask_suffix <- gsub("([\\.\\^\\$\\*\\+\\?\\(\\)\\{\\}\\|\\[\\]])", "\\\\\\1", mask_suffix)
    mask_pattern <- paste0(safe_mask_suffix, "$")
    mask_files_list <- list.files(mask_dir, pattern = mask_pattern, full.names = TRUE)

    image_files_list <- NULL
    if (is.null(image_suffix)) {
      if (shared_directory) stop("image_suffix cannot be NULL in shared directory mode.")
      image_pattern <- "\\.(png|tif|tiff|jpg|jpeg)$"
      image_files_list <- list.files(image_dir, pattern = image_pattern, ignore.case = TRUE, full.names = TRUE)
      # message("Using default image patterns (png, tif, jpg) as image_suffix is NULL.") # Removed
    } else {
      safe_image_suffix <- gsub("([\\.\\^\\$\\*\\+\\?\\(\\)\\{\\}\\|\\[\\]])", "\\\\\\1", image_suffix)
      image_pattern <- paste0(safe_image_suffix, "$")
      image_files_list <- list.files(image_dir, pattern = image_pattern, full.names = TRUE)
    }

    if (length(image_files_list) == 0) warning("No image files found matching criteria in: ", image_dir, call. = FALSE)
    if (length(mask_files_list) == 0) warning("No mask files found matching criteria in: ", mask_dir, call. = FALSE)

    # --- Match files ---
    get_base_name <- function(filename, suffix) {
      safe_suffix <- gsub("([\\.\\^\\$\\*\\+\\?\\(\\)\\{\\}\\|\\[\\]])", "\\\\\\1", suffix)
      return(sub(paste0(safe_suffix, "$"), "", basename(filename)))
    }

    mask_bases <- sapply(mask_files_list, get_base_name, suffix = mask_suffix)
    names(mask_bases) <- mask_files_list # Keep track of full paths

    image_bases <- NULL
    if (is.null(image_suffix)) {
      image_bases <- tools::file_path_sans_ext(basename(image_files_list))
    } else {
      image_bases <- sapply(image_files_list, get_base_name, suffix = image_suffix)
    }
    names(image_bases) <- image_files_list # Keep track of full paths

    # --- Report Missing Counterparts ---
    unmatched_image_bases <- setdiff(image_bases, mask_bases)
    unmatched_mask_bases <- setdiff(mask_bases, image_bases)

    if (length(unmatched_image_bases) > 0) {
      unmatched_image_files <- names(image_bases)[image_bases %in% unmatched_image_bases]
      warning("Found ", length(unmatched_image_files), " image file(s) with no matching mask file:\n ", paste(basename(unmatched_image_files), collapse="\n "), call. = FALSE)
    }
    if (length(unmatched_mask_bases) > 0) {
      unmatched_mask_files <- names(mask_bases)[mask_bases %in% unmatched_mask_bases]
      warning("Found ", length(unmatched_mask_files), " mask file(s) with no matching image file:\n ", paste(basename(unmatched_mask_files), collapse="\n "), call. = FALSE)
    }

    # Find matching pairs
    common_bases <- intersect(image_bases, mask_bases)
    if (length(common_bases) == 0 && (length(image_files_list) > 0 || length(mask_files_list) > 0)) {
      # Only warn if files were found but none matched
      warning("No matching image/mask pairs found based on base filenames and suffixes.", call. = FALSE)
    }
    # else { # Removed message about number found
    # message("Found ", length(common_bases), " matching image/mask pairs.")
    # }

    processed_count <- 0
    # Loop and process pairs
    for (base in common_bases) {
      img_idx <- which(image_bases == base)[1]
      mask_idx <- which(mask_bases == base)[1]
      img_file <- names(image_bases)[img_idx] # Get full path from names
      mask_file <- names(mask_bases)[mask_idx] # Get full path from names

      mask_basename <- basename(mask_file)
      output_basename <- sub(pattern = mask_pattern,
                             replacement = output_suffix,
                             x = mask_basename)
      if (output_basename == mask_basename) {
        warning(paste0("Mask suffix '", mask_suffix, "' not found in mask filename '", mask_basename, "'. Appending output suffix instead."), call. = FALSE)
        output_basename <- paste0(tools::file_path_sans_ext(mask_basename), output_suffix)
      }
      output_file_path <- file.path(output_target, output_basename)

      # message("Processing: ", basename(img_file), " + ", basename(mask_file), " -> ", basename(output_file_path)) # Removed per-file message
      created_file <- process_single_pair(img_file, mask_file, output_file_path, mode, outline_col, outline_lwd, overlay_col, overlay_alpha)
      if (!is.null(created_file)) {
        output_paths[[length(output_paths) + 1]] <- created_file
        processed_count <- processed_count + 1
      }
    }
    # message("Successfully processed ", processed_count, " out of ", length(common_bases), " matched pairs.") # Removed final summary message
  }

  # --- Return ---
  invisible(output_paths)
}

##############################################################################################################

#' Overlay Segmentation Masks onto Parent Images at Specific Coordinates
#'
#' @description Reads parent microscopy images, corresponding segmentation masks
#' (which match cropped regions), and a dataframe specifying the crop coordinates.
#' Creates output images showing the masks overlaid onto the parent images at the
#' correct locations, either as outlines or semi-transparent overlays.
#' Processes images in batches based on the parent image identifier found in the
#' coordinate dataframe. Uses the 'magick' package. Console output is minimized.
#'
#' @param parent_image_input Path to a directory containing the parent image files.
#' @param mask_input Path to a directory containing the segmentation mask files
#' (corresponding to cropped regions).
#' @param coord_df An R dataframe containing the mapping information. Must include
#' columns: 'Image_File' (base name used for both parent and mask files,
#' without suffix), 'X' (center x-coordinate of crop in parent), 'Y' (center
#' y-coordinate of crop in parent).
#' @param output_target Path to a directory where the composite parent images
#' (with overlays) will be saved.
#' @param mode Character string: "outline" or "overlay". Determines the
#' visualization style. Default is "outline".
#' @param parent_image_suffix Character string. Suffix appended to the `Image_File`
#' value to find parent image files. Default is `"_Run.png"`.
#' @param mask_suffix Character string. Suffix appended to the `Image_File`
#' value to find mask files. Default is `"_cp_masks.png"`.
#' @param output_suffix Character string. Suffix appended to the `Image_File`
#' value for the generated output files. Default is `"_overlayed.png"`.
#' @param outline_col Color for the outlines (used if mode="outline"). Default is "cyan".
#' @param outline_lwd Approximate line width (thickness) for outlines in pixels
#' (used if mode="outline"). Default is 2.
#' @param overlay_col Color for the overlay (used if mode="overlay"). Default is "yellow".
#' @param overlay_alpha Alpha transparency for the overlay (used if mode="overlay").
#' Default is 0.4.
#'
#' @return Invisibly returns a list of paths to the successfully created output files.
#' @export
#' @importFrom magick image_read image_info image_convert image_edge image_transparent image_blank image_composite image_write image_morphology image_fx
#' @importFrom grDevices col2rgb dev.off graphics.off
#' @importFrom graphics symbols
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' \dontrun{
#' # --- Setup Dummy Files/Dirs ---
#' if (requireNamespace("magick", quietly = TRUE) && requireNamespace("tools", quietly = TRUE)) {
#' # Create temp directories
#' temp_dir <- tempdir()
#' parent_dir <- file.path(temp_dir, "parent_images")
#' mask_dir <- file.path(temp_dir, "crop_masks")
#' out_dir <- file.path(temp_dir, "parent_output")
#' dir.create(parent_dir, showWarnings = FALSE)
#' dir.create(mask_dir, showWarnings = FALSE)
#' dir.create(out_dir, showWarnings = FALSE)
#'
#' # Define suffixes
#' parent_suffix <- "_microscopy.png" # Suffix for the parent image
#' mask_suffix <- "_mask.png" # Suffix for the mask image
#' output_suffix <- "_final_overlay.png"
#'
#' # Create dummy parent image
#' image_base <- "Experiment1_Tile1_RegionA" # Base name is the same
#' parent_path <- file.path(parent_dir, paste0(image_base, parent_suffix))
#' parent_img <- magick::image_blank(width = 500, height = 400, color = "grey80")
#' magick::image_write(parent_img, parent_path)
#'
#' # Create dummy mask file (representing a crop from the parent)
#' mask_path <- file.path(mask_dir, paste0(image_base, mask_suffix))
#' mask <- magick::image_blank(width = 50, height = 50, color = "black")
#' mask <- magick::image_draw(mask)
#' graphics::symbols(25, 25, circles = 15, inches = FALSE, add = TRUE, fg = "white", bg = "white")
#' grDevices::dev.off()
#' magick::image_write(mask, mask_path)
#'
#' # Create another mask for the same parent
#' image_base2 <- "Experiment1_Tile1_RegionA" # Same parent base name
#' mask_path2 <- file.path(mask_dir, paste0(image_base2, "_mask_2.png")) # Different mask suffix needed
#' mask2 <- magick::image_blank(width = 60, height = 40, color = "black")
#' mask2 <- magick::image_draw(mask2)
#' graphics::symbols(30, 20, rectangles = matrix(c(60,40),1,2), inches = FALSE,
#' add = TRUE, fg = "white", bg = "white")
#' grDevices::dev.off()
#' magick::image_write(mask2, mask_path2)
#'
#' # Create coordinate dataframe
#' # Image_File refers to the base name used for both parent and mask
#' coord_data <- data.frame(
#' Image_File = c(image_base, image_base2), # Base name identifier
#' X = c(100, 300), # Center X in parent
#' Y = c(150, 250) # Center Y in parent
#' )
#' # IMPORTANT: If multiple masks refer to the SAME parent image,
#' # they MUST have the same Image_File value in the dataframe.
#' # The mask_suffix parameter will be appended to this Image_File value
#' # to find the specific mask file. If masks for the same parent have different
#' # identifiers *within* their filename (like _mask_1, _mask_2), this needs
#' # a different approach (like the previous regex method or a dedicated mask ID column).
#' # Assuming here Image_File identifies the parent, and mask_suffix identifies the mask type.
#' # Let's adjust the example slightly for clarity if multiple masks per parent exist:
#'
#' # --- Revised Example Setup for Multiple Masks per Parent ---
#' # Parent Image
#' parent_base_name <- "Experiment1_Tile1"
#' parent_file_path <- file.path(parent_dir, paste0(parent_base_name, parent_suffix))
#' parent_img <- magick::image_blank(width = 500, height = 400, color = "grey80")
#' magick::image_write(parent_img, parent_file_path)
#'
#' # Mask 1 (associated with parent_base_name)
#' mask_id1 <- paste0(parent_base_name, "_crop1") # Unique ID for this mask
#' mask_path1 <- file.path(mask_dir, paste0(mask_id1, mask_suffix))
#' mask1 <- magick::image_blank(width = 50, height = 50, color = "black")
#' mask1 <- magick::image_draw(mask1); graphics::symbols(25, 25, circles = 15,
#' inches = FALSE, add = TRUE, fg = "white", bg = "white"); grDevices::dev.off()
#' magick::image_write(mask1, mask_path1)
#'
#' # Mask 2 (associated with parent_base_name)
#' mask_id2 <- paste0(parent_base_name, "_crop2") # Unique ID for this mask
#' mask_path2 <- file.path(mask_dir, paste0(mask_id2, mask_suffix))
#' mask2 <- magick::image_blank(width = 60, height = 40, color = "black")
#' mask2 <- magick::image_draw(mask2); graphics::symbols(30, 20, rectangles = matrix(c(60,40),1,2),
#' inches = FALSE, add = TRUE, fg = "white", bg = "white");
#' grDevices::dev.off()
#' magick::image_write(mask2, mask_path2)
#'
#' # Coordinate dataframe - Image_File now refers to the MASK ID
#' # We need a way to link back to the parent. Add Parent_Base column.
#' coord_data_revised <- data.frame(
#' Image_File = c(mask_id1, mask_id2), # ID for the mask file
#' Parent_Base = c(parent_base_name, parent_base_name), # Base name of parent
#' X = c(100, 300),
#' Y = c(150, 250)
#' )
#' # This revised structure seems necessary if Image_File cannot simultaneously
#' # identify the unique mask AND the parent. Let's proceed with this structure.
#'
#' # --- Run the function (using revised dataframe structure) ---
#' # Need to re-add Parent_Base column requirement
#' # Sticking to the user's clarification: Image_File IS the parent base.
#' # This implies only ONE mask file per parent can be processed this way,
#' # unless the coord_df has multiple rows with the SAME Image_File value
#' # but different X, Y (which implies multiple instances of the SAME mask overlayed).
#' # Let's assume the user means Image_File identifies the parent, and the mask
#' # file corresponding to that parent is found using Image_File + mask_suffix.
#'
#' coord_data_simple <- data.frame(
#' Image_File = c(parent_base_name), # Base name identifies parent AND the single mask
#' X = c(100),
#' Y = c(150)
#' )
#' # Create the corresponding mask file
#' mask_path_simple <- file.path(mask_dir, paste0(parent_base_name, mask_suffix))
#' magick::image_write(mask1, mask_path_simple) # Use mask1's content
#'
#' overlayMaskOnParent(
#' parent_image_input = parent_dir,
#' mask_input = mask_dir,
#' coord_df = coord_data_simple, # Use the simplified dataframe
#' output_target = out_dir,
#' mode = "overlay",
#' parent_image_suffix = parent_suffix,
#' mask_suffix = mask_suffix,
#' output_suffix = output_suffix,
#' overlay_col = "green",
#' overlay_alpha = 0.6
#' )
#'
#' print(paste("Output saved in:", out_dir))
#' list.files(out_dir) # Should show one file named Experiment1_Tile1_final_overlay.png
#'
#' # --- Clean up ---
#' unlink(parent_dir, recursive = TRUE)
#' unlink(mask_dir, recursive = TRUE)
#' unlink(out_dir, recursive = TRUE)
#' } else {
#' message("Magick and/or tools package not installed. Examples cannot run.")
#' }
#' }
overlayMaskOnParent <- function(parent_image_input,
                                mask_input,
                                coord_df,
                                output_target,
                                # parent_pattern_in_image_file, # Removed
                                mode = c("outline", "overlay"),
                                parent_image_suffix = "_Run.png",
                                mask_suffix = "_cp_masks.png",
                                output_suffix = "_overlayed.png",
                                outline_col = "cyan",
                                outline_lwd = 2,
                                overlay_col = "yellow",
                                overlay_alpha = 0.4) {

  # --- Argument Validation ---
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The 'magick' package is required but not installed.")
  }
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("The 'tools' package is required but not installed.")
  }
  # Removed stringr dependency check
  mode <- match.arg(mode)

  # Validate input types and existence
  if (!dir.exists(parent_image_input)) stop("parent_image_input must be an existing directory.")
  if (!dir.exists(mask_input)) stop("mask_input must be an existing directory.")
  if (!is.data.frame(coord_df)) stop("coord_df must be an R dataframe.")
  if (!dir.exists(output_target)) {
    message("Output directory '", output_target, "' does not exist. Creating it.")
    dir.create(output_target, recursive = TRUE, showWarnings = FALSE)
  }
  # Removed regex pattern validation

  # Validate coord_df columns
  required_cols <- c("Image_File", "X", "Y") # Expecting Image_File to identify parent/mask base
  missing_cols <- setdiff(required_cols, colnames(coord_df))
  if (length(missing_cols) > 0) {
    stop("coord_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.numeric(coord_df$X) || !is.numeric(coord_df$Y)) {
    stop("Columns 'X' and 'Y' in coord_df must be numeric.")
  }
  if (!is.character(coord_df$Image_File)) {
    stop("Column 'Image_File' in coord_df must be character strings.")
  }

  # Validate other parameters (colors, numbers, suffixes)
  is_valid_color <- function(color_string) {
    tryCatch({ grDevices::col2rgb(color_string); TRUE }, error = function(e) FALSE)
  }
  if (!is_valid_color(outline_col)) stop("Invalid color specified for outline_col: ", outline_col)
  if (!is_valid_color(overlay_col)) stop("Invalid color specified for overlay_col: ", overlay_col)
  if (!is.numeric(outline_lwd) || length(outline_lwd)!= 1 || outline_lwd <= 0) {
    stop("outline_lwd must be a single positive number.")
  }
  if (!is.numeric(overlay_alpha) || length(overlay_alpha)!= 1 || overlay_alpha < 0 || overlay_alpha > 1) {
    stop("overlay_alpha must be a single number between 0 and 1.")
  }
  if (!is.character(parent_image_suffix) || length(parent_image_suffix) != 1) {
    stop("parent_image_suffix must be a single character string.")
  }
  if (!is.character(mask_suffix) || length(mask_suffix) != 1) {
    stop("mask_suffix must be a single character string.")
  }
  if (!is.character(output_suffix) || length(output_suffix) != 1) {
    stop("output_suffix must be a single character string.")
  }

  # --- Group coordinates by Parent File (using Image_File column) ---
  # Check for duplicate Image_File entries if multiple masks per parent isn't intended
  if (any(duplicated(coord_df$Image_File))) {
    warning("Duplicate entries found in 'Image_File' column. If each parent should only have one mask overlay processed via this function, check coord_df. Processing all entries.", call. = FALSE)
  }

  # Group by the Image_File column, assuming it identifies the parent
  parent_groups <- split(coord_df, coord_df$Image_File)
  unique_parent_bases <- names(parent_groups) # These are the values from Image_File

  output_paths <- list()
  processed_parent_count <- 0
  total_masks_processed_successfully <- 0

  message("Found ", length(unique_parent_bases), " unique parent image identifier(s) in coord_df$Image_File.")

  # --- Loop through each Parent Image ---
  for (parent_base in unique_parent_bases) { # parent_base is now the value from Image_File
    # Construct parent file path using the Image_File value
    parent_file_name <- paste0(parent_base, parent_image_suffix)
    parent_file_path <- file.path(parent_image_input, parent_file_name)
    parent_coords <- parent_groups[[parent_base]] # Get rows for this parent identifier

    if (!file.exists(parent_file_path)) {
      warning("Parent image file not found for identifier '", parent_base, "', skipping: ", parent_file_path, call. = FALSE)
      next # Skip to the next parent
    }

    current_parent_state <- NULL
    parent_processed_successfully = TRUE # Flag for this parent

    tryCatch({
      # Read the parent image ONCE
      current_parent_state <- magick::image_read(parent_file_path)
      info_parent <- magick::image_info(current_parent_state)[1,]

      masks_processed_for_this_parent <- 0

      # --- Loop through entries associated with this parent identifier ---
      # This loop handles cases where the same parent ID appears multiple times
      # in the dataframe (e.g., to overlay the same mask at different locations,
      # or if the user *did* intend multiple different masks linked only by the parent ID)
      for (i in 1:nrow(parent_coords)) {
        coord_info <- parent_coords[i, ]
        # Use the Image_File value (which is parent_base) to find the mask
        mask_file_name <- paste0(parent_base, mask_suffix)
        mask_file_path <- file.path(mask_input, mask_file_name)

        if (!file.exists(mask_file_path)) {
          # More specific warning since we assume one mask per parent ID here
          warning("Mask file not found for parent identifier '", parent_base,
                  "', skipping overlay at X=", coord_info$X, ", Y=", coord_info$Y, ". Looked for: ", mask_file_path, call. = FALSE)
          next # Skip this specific overlay instance
        }

        mask_img <- NULL
        overlay_layer_final <- NULL
        mask_processed_ok <- FALSE

        # --- Generate overlay layer for this mask ---
        tryCatch({
          mask_img <- magick::image_read(mask_file_path)
          info_mask <- magick::image_info(mask_img)[1, ]

          # Calculate top-left offset
          offset_x <- floor(coord_info$X - (info_mask$width / 2))
          offset_y <- floor(coord_info$Y - (info_mask$height / 2))

          # Boundary Check
          if (offset_x < 0 || offset_y < 0 || (offset_x + info_mask$width) > info_parent$width || (offset_y + info_mask$height) > info_parent$height) {
            warning("Mask '", basename(mask_file_path), "' at coordinates (X=", coord_info$X, ", Y=", coord_info$Y,
                    ") extends beyond parent '", basename(parent_file_path), "' dimensions. Skipping this overlay.", call. = FALSE)
            next
          }
          offset_string <- sprintf("%+d%+d", offset_x, offset_y)

          # --- Core magick logic to create overlay layer (size of mask) ---
          if (mode == "outline") {
            mask_gray <- magick::image_convert(mask_img, type = 'grayscale')
            edges <- magick::image_edge(mask_gray, radius = 1)
            kernel_size <- max(1, round(outline_lwd / 2))
            edges_thick <- magick::image_morphology(edges, method = 'Dilate', kernel = paste0('Disk:', kernel_size))
            color_layer <- magick::image_blank(width = info_mask$width, height = info_mask$height, color = outline_col)
            edges_thick_mask <- magick::image_convert(edges_thick, type = 'grayscale')
            overlay_layer_final <- magick::image_composite(
              image = color_layer,
              composite_image = edges_thick_mask,
              operator = "CopyOpacity"
            )
          } else { # mode == "overlay"
            mask_transparent <- magick::image_transparent(mask_img, color = "black", fuzz = 10)
            rgb_vals <- grDevices::col2rgb(overlay_col)
            semi_transparent_color_string <- sprintf("rgba(%d, %d, %d, %f)",
                                                     rgb_vals[1], rgb_vals[2], rgb_vals[3],
                                                     overlay_alpha)
            semi_transparent_color_layer <- magick::image_blank(width = info_mask$width,
                                                                height = info_mask$height,
                                                                color = semi_transparent_color_string)
            overlay_layer_final <- magick::image_composite(
              image = semi_transparent_color_layer,
              composite_image = mask_transparent,
              operator = "In"
            )
          }

          # --- Composite this single overlay onto the current parent state ---
          current_parent_state <- magick::image_composite(
            image = current_parent_state,
            composite_image = overlay_layer_final,
            operator = "Over",
            offset = offset_string
          )
          mask_processed_ok <- TRUE

        }, error = function(e_mask) {
          warning(paste("Failed to process mask '", basename(mask_file_path), "' for parent '",
                        basename(parent_file_path), "' at X=", coord_info$X, ", Y=", coord_info$Y,
                        ". Skipping this overlay.\n Error: ", e_mask$message), call. = FALSE)
        }) # End tryCatch for single mask processing

        if(mask_processed_ok) {
          masks_processed_for_this_parent <- masks_processed_for_this_parent + 1
        }

      } # End inner loop through masks/coordinates for this parent

      # --- Save the final parent image if at least one overlay was processed ---
      if (masks_processed_for_this_parent > 0) {
        # Use the parent_base (from Image_File) for the output filename
        output_base_name <- paste0(parent_base, output_suffix)
        output_file_path <- file.path(output_target, output_base_name)

        magick::image_write(current_parent_state, path = output_file_path, format = "png")
        output_paths[[length(output_paths) + 1]] <- output_file_path
        total_masks_processed_successfully <- total_masks_processed_successfully + masks_processed_for_this_parent
      } else {
        warning("No overlays were successfully processed for parent: ", basename(parent_file_path), call. = FALSE)
        parent_processed_successfully = FALSE
      }

    }, error = function(e_parent) {
      warning(paste("Failed to process parent image '", basename(parent_file_path), "'. Skipping this parent.\n Error: ", e_parent$message), call. = FALSE)
      parent_processed_successfully = FALSE
    }) # End tryCatch for parent processing

    if(parent_processed_successfully && masks_processed_for_this_parent > 0) {
      processed_parent_count <- processed_parent_count + 1
    }

  } # End outer loop through parent images

  message("Finished processing. Successfully generated ", length(output_paths), " output image(s) for ", processed_parent_count, " parent image(s), incorporating ", total_masks_processed_successfully, " mask overlays.")

  # --- Return ---
  invisible(output_paths)
}
