### Functions for processing cellenONE images, and overlaying masks produced by cellProfiler onto these images

##############################################################################################################
#' cropImages
#'
#' Reads images specified in a data frame, crops them based on a user-defined
#' offset centred on user-provided pixel coordinates, and saves the cropped images to a specified
#' output directory. Allows specifying a base input directory if image paths
#' in the data frame are relative filenames.
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
