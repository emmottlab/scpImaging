% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imageAnalysis.R
\name{addCellProfilerAttributesSCE}
\alias{addCellProfilerAttributesSCE}
\title{'addCellProfilerAttributesSCE()': Add CellProfiler Attributes to colData of Bioconductor Objects}
\usage{
addCellProfilerAttributesSCE(
  sce_obj,
  df2,
  key1_col = "Cropped",
  key2_col = "FileName_Image",
  multiplet_handling = c("voidmultiplets", "takemaxarea"),
  area_col = "AreaShape_Area"
)
}
\arguments{
\item{sce_obj}{A \code{SummarizedExperiment}, \code{SingleCellExperiment}, or
\code{QFeatures} object.}

\item{df2}{The secondary data frame (data.frame or tibble) containing the
attributes to add. Passed directly to \code{addCellProfilerAttributes}.}

\item{key1_col}{The name of the key column \emph{in the \code{colData} of \code{sce_obj}}
(character string). Defaults to \code{"Cropped"}. Passed to
\code{addCellProfilerAttributes}.}

\item{key2_col}{The name of the key column in \code{df2} (character string).
Defaults to \code{"FileName_Image"}. Passed to \code{addCellProfilerAttributes}.}

\item{multiplet_handling}{Method to handle multiple rows for the same key in \code{df2}.
Must be one of \code{"voidmultiplets"} or \code{"takemaxarea"} (character string).
Defaults to \code{"voidmultiplets"}. Passed to \code{addCellProfilerAttributes}.}

\item{area_col}{The name of the column in \code{df2} containing the area values,
required only if \code{multiplet_handling = "takemaxarea"} (character string).
Defaults to \code{"AreaShape_Area"}. Passed to \code{addCellProfilerAttributes}.}
}
\value{
An object of the same class as \code{sce_obj} with its \code{colData} updated
to include the merged attributes from \code{df2}.
}
\description{
A wrapper function for \code{addCellProfilerAttributes} that operates on the
\code{colData} of \code{SummarizedExperiment}, \code{SingleCellExperiment}, or \code{QFeatures}
objects.
}
\details{
This function extracts the \code{colData} from the input object (\code{sce_obj}),
converts it to a standard \code{data.frame}, and passes it as \code{df1} to the
\code{addCellProfilerAttributes} function along with \code{df2} and other parameters.
The resulting merged data frame is then converted back to a \code{DataFrame} and
used to replace the original \code{colData} in the input object.

The row names of the \code{colData} are preserved throughout the process.
For \code{QFeatures} objects, this function modifies the primary (top-level)
\code{colData}.

See \code{?addCellProfilerAttributes} for details on the merging logic,
\code{multiplet_handling} options, and other parameters.
}
\examples{

# --- Create Sample SummarizedExperiment ---
library(SummarizedExperiment)
library(S4Vectors)
counts <- matrix(rpois(100, lambda = 10), ncol=10, nrow=10)
rownames(counts) <- paste0("Gene", 1:10)
colnames(counts) <- paste0("Cell", 1:10)

sample_coldata <- DataFrame(
   SampleID = paste0("S", 1:10),
   Treatment = rep(c("A", "B"), 5),
   # Key column matching df1's perspective in addCellProfilerAttributes
   Cropped_Path = paste0("path/", letters[1:10]),
   row.names = colnames(counts) # Ensure row names match assay colnames
)

sce <- SummarizedExperiment(assays=list(counts=counts),
                            colData=sample_coldata)

print("Original colData:")
print(colData(sce))

# --- Create Sample df2 (attributes to add) ---
df2_attribs <- data.frame(
  FileName_Image = c("path/a", "path/b", "path/b", "path/c", "path/f", "path/g"),
  QC_metric = rnorm(6, mean=100),
  AreaShape_Area = c(50, 60, 55, 70, 80, 90),
  Batch = rep(c("X", "Y"), 3)
)
print("Attributes to add (df2):")
print(df2_attribs)

}
