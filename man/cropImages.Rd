% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imageProcessing.R
\name{cropImages}
\alias{cropImages}
\title{'cropImages()': Generate Cropped Images from Image Paths and Coordinates}
\usage{
cropImages(
  df,
  pixel_offset,
  output_dir,
  input_dir = NULL,
  output_prefix = "cropped_",
  output_suffix = NULL,
  create_dir = FALSE
)
}
\arguments{
\item{df}{A data frame containing cellenONE-derived image metadata. For example derived from a cell sorting file
generated by the cellenONE, or a sampleannotation file generated by scpAnnotator. \strong{Required.}
Must include columns: \code{ImageFile} (character, full path to the input image
or filename relative to \code{input_dir} if provided),
\code{X} (numeric, x-coordinate of the center point of the cell for cropping, as identified by the cellenONE),
\code{Y} (numeric, y-coordinate of the center point of the cell for cropping, as identified by the cellenONE).}

\item{pixel_offset}{An integer specifying the number of pixels to offset from
the center coordinates (\code{X}, \code{Y}) in each direction (up, down,
left, right) to define the cropping box. \strong{Required.} The resulting crop
dimensions will be (2 * $pixel_offset$) x (2 * $pixel_offset$). Must be a
positive integer. A pixel offset of 37, will create a 75 pixel crop (37 + centre pixel, + 37).}

\item{output_dir}{A character string specifying the path to the directory
where cropped images should be saved. \strong{Required.}}

\item{input_dir}{An optional character string specifying a base directory
for input images. If provided, paths in \code{df$ImageFile} are treated as
relative to this directory (e.g., if \code{input_dir} is "/path/to/images" and
\code{df$ImageFile} contains "img1.png", the function will look for
"/path/to/images/img1.png"). If \code{NULL} (the default), \code{df$ImageFile}
must contain full paths or paths relative to the current working directory.}

\item{output_prefix}{A character string to add to the beginning of the output filenames.
Defaults to `"cropped_"'. Optional. \strong{Strongly recommend} leaving unchanged for consistency.}

\item{output_suffix}{An optional character string used to replace the original file
extension in the output filename. For example, if \code{output_suffix} is \code{"_cropped.png"},
an input file \code{image1.tif} will result in an output file named \code{image1_cropped.png}
(plus any \code{output_prefix}). If the original filename has no extension, the suffix
is simply appended. If \code{NULL} or \code{""} (the default), the original filename
(including its extension) from \code{ImageFile} is used as the base for the output filename.
\strong{Strongly recommend} leaving unchanged for consistency.}

\item{create_dir}{A logical value indicating whether the \code{output_dir} should
be created if it does not exist. Defaults to \code{FALSE}. If \code{TRUE}, the
function will attempt to create the directory recursively. If \code{FALSE} and
the directory does not exist, the function will stop with an error. Optional.}
}
\value{
Returns \code{NULL}, so does not create an object in the R environment. The function's primary effect is saving
cropped image files to the specified \code{output_dir}.
}
\description{
Reads images specified in a data frame, crops them based on a user-defined
offset centred on user-provided pixel coordinates, and saves the cropped images to a specified
output directory. Allows specifying a base input directory if image paths
in the data frame are relative filenames. Output filenames are derived from the
input \code{ImageFile} names, with options to add a prefix and replace the file extension
with a custom suffix. For the intended use case, the dataframe
represents the cellenONE cell sorting output file containing cell metadata and image names,
the images are the images recorded for each sorted cell, and the XY coordinates represent
the location of the cell in the image. The function crops a square region surrounding the cell
of interest, with the size of the square determined by the pixel offset.
}
