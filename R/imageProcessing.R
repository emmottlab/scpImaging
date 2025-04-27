### Functions for processing cellenONE images, and overlaying masks produced by cellpose onto these images

# Currently this includes:
# 1. cropImages() - Status: functional (check more)
# 2. npy_masks_to_png() - Status: functional
# 3. overlayMask()
# 4. overlayMaskOnParent() - TODO.

##############################################################################################################

#' 'cropImages()' generates cropped images from a data frame of image paths and XY coordinates
#'
#' Reads images specified in a data frame, crops them based on a user-defined
#' offset centred on user-provided pixel coordinates, and saves the cropped images to a specified
#' output directory. Allows specifying a base input directory if image paths
#' in the data frame are relative filenames. For the intended use case, the dataframe
#' represents the cellenONE cell sorting output file containing cell metadata and image names,
#' the images are the images recorded for each sorted cell, and the XY coordinates represent
#' the location of the cell in the image. The function crops a square region surrounding the cell
#' of interest, with the size of the square determined by the pixel offset.
#'
#' @param df A data frame containing image processing information. **Required.**
#'   Must include columns: `image_path` (character, full path to the input image
#'   or filename relative to `input_dir` if provided),
#'   `x_coord` (numeric, x-coordinate of the center point for cropping),
#'   `y_coord` (numeric, y-coordinate of the center point for cropping),
#'   `output_filename` (character, desired filename for the cropped output image).
#' @param pixel_offset An integer specifying the number of pixels to offset from
#'   the center coordinates (`x_coord`, `y_coord`) in each direction (up, down,
#'   left, right) to define the cropping box. **Required.** The resulting crop
#'   dimensions will be (2 * pixel_offset) x (2 * pixel_offset). Must be a
#'   positive integer.
#' @param output_dir A character string specifying the path to the directory
#'   where cropped images should be saved. **Required.**
#' @param input_dir An optional character string specifying a base directory
#'   for input images. If provided, paths in `df$image_path` are treated as
#'   relative to this directory (e.g., if `input_dir` is "/path/to/images" and
#'   `df$image_path` contains "img1.png", the function will look for
#'   "/path/to/images/img1.png"). If `NULL` (the default), `df$image_path`
#'   must contain full paths or paths relative to the current working directory.
#' @param output_prefix A character string to prepend to the output filenames
#'   specified in the `df$output_filename` column. Defaults to `""` (an empty
#'   string), meaning no prefix is added by default. Optional.
#' @param create_dir A logical value indicating whether the `output_dir` should
#'   be created if it does not exist. Defaults to `FALSE`. If `TRUE`, the
#'   function will attempt to create the directory recursively. If `FALSE` and
#'   the directory does not exist, the function will stop with an error. Optional.
#'
#' @return Invisibly returns `NULL`. The function's primary effect is saving
#'   cropped image files to the specified `output_dir`.
#' @export
#' @importFrom magick image_read image_crop image_write image_info

cropImages <- function(df, pixel_offset, output_dir, input_dir = NULL, output_prefix = "", create_dir = FALSE) {

  # --- Input Validation ---
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }
  required_cols <- c("image_path", "x_coord", "y_coord", "output_filename")
  if (!all(required_cols %in% names(df))) {
    stop("`df` must contain columns: ", paste(required_cols, collapse = ", "))
  }
  if (!is.numeric(pixel_offset) || length(pixel_offset)!= 1 ||  pixel_offset <= 0 || floor(pixel_offset)!= pixel_offset) {
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
    # Determine the full input image path
    relative_image_path <- df$image_path[i]
    if (!is.null(input_dir)) {
      current_image_path <- file.path(input_dir, relative_image_path)
    } else {
      current_image_path <- relative_image_path
    }

    x_coord <- df$x_coord[i]
    y_coord <- df$y_coord[i]
    output_filename <- df$output_filename[i]

    # Validate row-specific inputs (using relative_image_path for clarity in messages if input_dir is used)
    if (is.na(relative_image_path) ||!is.character(relative_image_path) || nchar(relative_image_path) == 0) {
      warning("Row ", i, ": Invalid or missing `image_path` value. Skipping.")
      next
    }
    if (is.na(x_coord) ||!is.numeric(x_coord) || is.na(y_coord) ||!is.numeric(y_coord)) {
      warning("Row ", i, ": Invalid or missing coordinates (`x_coord`, `y_coord`) for image '", relative_image_path, "'. Skipping.")
      next
    }
    if (is.na(output_filename) ||!is.character(output_filename) || nchar(output_filename) == 0) {
      warning("Row ", i, ": Invalid or missing `output_filename` for image '", relative_image_path, "'. Skipping.")
      next
    }

    # Check if constructed input image file exists
    if (!file.exists(current_image_path)) {
      warning("Row ", i, ": Image file not found at '", current_image_path, "'. Skipping.")
      next
    }

    tryCatch({
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

      # Construct output path
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

#' 'overlayMask()' Overlay Segmentation Mask on Image
#'
#' @description Reads a microscopy image and a corresponding segmentation mask,
#'   then creates an output image showing the mask as either outlines or a
#'   semi-transparent overlay on the original image.
#'
#' @param image_path Path to the original microscopy image (*.png).
#' @param mask_path Path to the segmentation mask image (*.png).
#'   Expected to be binary or label image where background is black (intensity 0)
#'   and segmented regions are non-black.
#' @param output_path Path to save the resulting composite image (PNG format).
#' @param mode Character string: "outline" or "overlay". Determines the visualization style.
#' @param outline_col Color for the outlines (used if mode="outline"). Can be any
#'   format recognized by `magick` (e.g., "cyan", "#00FFFF"). Default is "cyan".
#' @param outline_lwd Approximate line width (thickness) for outlines in pixels
#'   (used if mode="outline"). Default is 2.
#' @param overlay_col Color for the overlay (used if mode="overlay"). Can be any
#'   format recognized by `magick`. Default is "yellow".
#' @param overlay_alpha Alpha transparency for the overlay (used if mode="overlay").
#'   A numeric value between 0 (fully transparent) and 1 (fully opaque). Default is 0.4.
#'
#' @return Invisibly returns the `output_path` where the composite image was saved.
#' @export
#' @importFrom magick image_read image_info image_convert image_edge image_transparent image_blank image_composite image_write image_morphology
#' @importFrom grDevices col2rgb dev.off graphics.off
#' @importFrom graphics symbols
#'
#' @examples
#' \dontrun{
#' # Requires the 'magick' package
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   # Create dummy files for example
#'   img <- magick::image_blank(width=100, height=100, color="grey")
#'   mask <- magick::image_blank(width=100, height=100, color="black")
#'
#'   # Draw a white circle on the mask using magick's drawing capabilities
#'   mask <- magick::image_draw(mask)
#'   graphics::symbols(50, 50, circles=25, inches=FALSE, add=TRUE, fg="white", bg="white")
#'   grDevices::dev.off() # Close the drawing device opened by image_draw
#'
#'   img_path <- tempfile(fileext = ".png")
#'   mask_path <- tempfile(fileext = ".png")
#'   out_outline <- tempfile(fileext = ".png")
#'   out_overlay <- tempfile(fileext = ".png")
#'
#'   magick::image_write(img, img_path)
#'   magick::image_write(mask, mask_path)
#'
#'   # Generate outline version
#'   overlayMask(img_path, mask_path, out_outline, mode = "outline",
#'                outline_col = "lime", outline_lwd = 3)
#'   print(paste("Outline image saved to:", out_outline))
#'
#'   # Generate overlay version
#'   overlayMask(img_path, mask_path, out_overlay, mode = "overlay",
#'                overlay_col="magenta", overlay_alpha = 0.6)
#'   print(paste("Overlay image saved to:", out_overlay))
#'
#'   # Optional: View the first image if running interactively
#'   # if(interactive()) print(magick::image_read(out_outline))
#'
#'   # Clean up temporary files
#'   file.remove(img_path, mask_path, out_outline, out_overlay)
#' } else {
#'   message("Magick package not installed. Example cannot run.")
#' }
#' }
overlayMask <- function(image_path,
                         mask_path,
                         output_path,
                         mode = c("outline", "overlay"),
                         outline_col = "cyan",
                         outline_lwd = 2,
                         overlay_col = "yellow",
                         overlay_alpha = 0.4) {

  # --- Argument Validation ---
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The 'magick' package is required but not installed.")
  }

  if (!is.character(image_path) || length(image_path)!= 1 ||!file.exists(image_path)) {
    stop("Input image file not found or path is invalid: ", image_path)
  }
  if (!is.character(mask_path) || length(mask_path)!= 1 ||!file.exists(mask_path)) {
    stop("Input mask file not found or path is invalid: ", mask_path)
  }
  if (!is.character(output_path) || length(output_path)!= 1) {
    stop("Output path must be a single character string.")
  }
  # Ensure output directory exists (optional, or let image_write handle it)
  # out_dir <- dirname(output_path)
  # if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  mode <- match.arg(mode) # Ensures mode is either "outline" or "overlay"

  # Validate colors
  is_valid_color <- function(color_string) {
    tryCatch({
      grDevices::col2rgb(color_string)
      TRUE
    }, error = function(e) FALSE)
  }
  if (!is_valid_color(outline_col)) stop("Invalid color specified for outline_col: ", outline_col)
  if (!is_valid_color(overlay_col)) stop("Invalid color specified for overlay_col: ", overlay_col)


  if (!is.numeric(outline_lwd) || length(outline_lwd)!= 1 || outline_lwd <= 0) {
    stop("outline_lwd must be a single positive number.")
  }
  if (!is.numeric(overlay_alpha) || length(overlay_alpha)!= 1 || overlay_alpha < 0 || overlay_alpha > 1) {
    stop("overlay_alpha must be a single number between 0 and 1.")
  }

  # --- Image Loading and Validation ---
  original_img <- tryCatch({
    magick::image_read(image_path)
  }, error = function(e) {
    stop("Failed to read original image '", image_path, "': ", e$message)
  })

  mask_img <- tryCatch({
    magick::image_read(mask_path)
  }, error = function(e) {
    stop("Failed to read mask image '", mask_path, "': ", e$message)
  })

  # Check dimensions
  info_orig <- magick::image_info(original_img)[1, ] # Ensure we only get info for first frame if multi-frame
  info_mask <- magick::image_info(mask_img)[1, ]
  if (info_orig$width!= info_mask$width || info_orig$height!= info_mask$height) {
    stop("Original image and mask dimensions do not match (Image: ",
         info_orig$width, "x", info_orig$height, ", Mask: ",
         info_mask$width, "x", info_mask$height, ").")
  }

  # --- Image Processing ---
  final_image <- NULL

  ####
  if (mode == "outline") {
    # 1. Detect edges
    mask_gray <- magick::image_convert(mask_img, type = 'grayscale')
    edges <- magick::image_edge(mask_gray, radius = 1)

    # 2. Thicken edges
    kernel_size <- max(1, round(outline_lwd / 2))
    edges_thick <- magick::image_morphology(edges, method = 'Dilate',
                                            kernel = paste0('Disk:', kernel_size))

    # --- CORRECTED STEPS 3 & 4 ---
    # 3. Create the colored outline layer with transparency
    # Create a solid color image for the outline color
    color_layer <- magick::image_blank(width = info_orig$width,
                                       height = info_orig$height,
                                       color = outline_col)

    # Ensure the thickened edges image is grayscale to be used as an alpha mask
    edges_thick_mask <- magick::image_convert(edges_thick, type = 'grayscale')

    # Use the 'CopyOpacity' composite operator. This takes the grayscale intensity
    # of the second image (edges_thick_mask) and uses it to set the alpha
    # channel of the first image (color_layer). Where edges_thick_mask is white,
    # color_layer becomes opaque. Where edges_thick_mask is black, color_layer
    # becomes transparent.
    colored_outlines_with_alpha <- magick::image_composite(
      image = color_layer, # The image whose alpha channel is modified
      composite_image = edges_thick_mask, # The image providing the alpha values
      operator = "CopyOpacity" #
    )

    # 4. Composite the outline layer (which now has transparency) onto the original image
    final_image <- magick::image_composite(
      image = original_img, # Base image
      composite_image = colored_outlines_with_alpha, # Outlines with transparency
      operator = "Over" # Standard overlay operator
    )
    # --- END OF CORRECTED STEPS ---

  } else { # mode == "overlay"
    # (Overlay logic remains the same)
    # 1. Make mask background transparent
    mask_transparent <- magick::image_transparent(mask_img, color = "black", fuzz = 10) # Fuzz might be less critical here but often harmless

    # 2. Create a solid color layer for the overlay
    color_layer <- magick::image_blank(width = info_orig$width,
                                       height = info_orig$height,
                                       color = overlay_col)

    # 3. Composite using Blend operator with alpha and mask
    blend_percentage <- overlay_alpha * 100
    final_image <- magick::image_composite(original_img,
                                           color_layer,
                                           operator = "Blend",
                                           compose_args = as.character(blend_percentage),
                                           mask = mask_transparent)
  }

  # --- Save Output ---
  # (Saving logic remains the same)
  tryCatch({
    magick::image_write(final_image, path = output_path, format = "png")
  }, error = function(e) {
    stop("Failed to write output image to '", output_path, "': ", e$message)
  })

  # --- Return Output Path Invisibly ---
  invisible(output_path)
}


##############################################################################################################

# Version of the above, where it overlays the mask on the parent image.
