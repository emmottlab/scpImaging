% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imageAnalysis.R
\name{countCells}
\alias{countCells}
\title{'countCells()':Count Unique Masks in PNG Segmentation Masks}
\usage{
countCells(
  path = NULL,
  filelist = NULL,
  suffix = "_cp_masks.png",
  expected_bit_depth = 8
)
}
\arguments{
\item{path}{A character string specifying the path to the directory
containing the PNG mask files. Mutually exclusive with \code{filelist}.
Defaults to \code{NULL}.}

\item{filelist}{A character vector of full file paths to the PNG mask files.
Mutually exclusive with \code{path}. Defaults to \code{NULL}.}

\item{suffix}{A character string specifying the filename suffix (pattern)
to match when \code{path} is provided. The pattern is matched at the end of
the filename. Defaults to \verb{_cp_masks.png}. Ignored if \code{filelist} is provided.}

\item{expected_bit_depth}{A numeric value indicating the expected bit depth
of the original PNG mask files (typically 8 or 16). This is used to
interpret the raw pixel data correctly. Defaults to 8.}
}
\value{
A dataframe with two columns:
\item{FileName}{Character, the base name of the processed PNG file.}
\item{CP_CellCount}{Integer, the number of unique positive integer mask
values found in the image. Contains \code{NA} if the file could not be
processed.}
}
\description{
Processes PNG segmentation masks generated from cellpose .npy files by
counting the number of unique positive integer values.
These values correspond to individual segmented objects (e.g., cells),
while background pixels (value 0) are ignored.
The function can handle input specified as a directory containing masks, an
explicit list of mask file paths, or a directory and a filename suffix.
}
\details{
This function reads PNG files using \code{magick::image_read}. It then uses
\code{magick::image_data} to extract the raw pixel data for the grayscale
channel.

Because \code{magick::image_info} may not reliably return the bit depth for all
PNG files, this function uses the \code{expected_bit_depth} argument to interpret
the pixel data (bytes):
\itemize{
\item If \code{expected_bit_depth} is 8, raw bytes directly represent integer values (0-255).
\item If \code{expected_bit_depth} is 16, pairs of raw bytes are combined to reconstruct the
16-bit integer values (0-65535), assuming big-endian byte order.
}

The function assumes that the input PNGs are grayscale integer images where
different masks are represented by unique positive integers and the
background is 0. If non-grayscale images are encountered, a warning is
issued, and the function attempts to process the grayscale information.

The function includes error handling: if a file cannot be read or processed
(e.g., corrupted file, non-PNG format, unsupported expected bit depth),
a warning is issued, and \code{NA} is returned for that file's count.

Provide \emph{either} \code{path} (optionally with \code{suffix}) \emph{or} \code{filelist}, but not both.
}
\examples{
\dontrun{
# Example Setup: Create dummy PNG mask files using the 'magick' package
temp_dir <- tempdir()
# Mask 1: 2 objects (IDs 1, 2)
mask1_data <- matrix(c(0, 0, 1, 1, 0, 2, 2, 0, 0), nrow = 3, byrow = TRUE)
# Mask 2: 4 objects (IDs 1, 2, 3, 4)
mask2_data <- matrix(c(1, 1, 1, 0, 0, 0, 2, 3, 4), nrow = 3, byrow = TRUE)
# Mask 3: 3 objects (IDs 10, 20, 300) - requires 16-bit
mask3_data_16bit <- matrix(c(0, 10, 20, 300), nrow = 2, byrow = TRUE)

# Create magick images from matrices (scale to 0-1 for image_read)
# Save as 16-bit PNGs to handle mask3
max_val_16bit <- 65535
img1_magick <- magick::image_read(mask1_data / max_val_16bit)
img2_magick <- magick::image_read(mask2_data / max_val_16bit)
img3_magick <- magick::image_read(mask3_data_16bit / max_val_16bit)

# Set depth to 16-bit before writing
img1_magick <- magick::image_depth(img1_magick, 16)
img2_magick <- magick::image_depth(img2_magick, 16)
img3_magick <- magick::image_depth(img3_magick, 16)

# Write PNGs using magick
magick::image_write(img1_magick,
                    path = file.path(temp_dir, "imageA_masks.png"),
                    format = "png")
magick::image_write(img2_magick,
                    path = file.path(temp_dir, "imageB_masks.png"),
                    format = "png")
magick::image_write(img3_magick,
                    path = file.path(temp_dir, "imageC_16bit_masks.png"),
                    format = "png")

# Create a non-png file to test error handling
writeLines("not a png", file.path(temp_dir, "not_a_png.txt"))

# --- Usage Examples ---

# 1. Using path and default suffix (".png") and default depth (16)
counts_path_default <- countCells(path = temp_dir)
print(counts_path_default)
# Expected output: Counts for A, B, C (2, 4, 3 respectively)

# 2. Using path and specific suffix, explicitly setting 16-bit depth
counts_path_suffix <- countCells(path = temp_dir,
                                            suffix = "_masks.png",
                                            expected_bit_depth = 16)
print(counts_path_suffix)
# Expected output: Counts for A, B, C (2, 4, 3 respectively)

# 3. Using filelist (including non-png and non-existent files to show NA)
file_paths <- c(
  file.path(temp_dir, "imageA_masks.png"),
  file.path(temp_dir, "imageB_masks.png"),
  file.path(temp_dir, "imageC_16bit_masks.png"),
  file.path(temp_dir, "non_existent_file.png"), # Test non-existent file
  file.path(temp_dir, "not_a_png.txt")          # Test invalid file
)
counts_filelist <- countCells(filelist = file_paths,
                                         expected_bit_depth = 16)
print(counts_filelist)
# Expected output: Counts for A, B, C, and NA for the last two with warnings.

# 4. Example assuming masks were 8-bit (will misinterpret C)
#    (Need to recreate test files as 8-bit for this to be accurate)
# counts_8bit <- countCells(path = temp_dir,
#                                      suffix = "_masks.png",
#                                      expected_bit_depth = 8)
# print(counts_8bit)

# Clean up temporary files
unlink(temp_dir, recursive = TRUE)
}

}
