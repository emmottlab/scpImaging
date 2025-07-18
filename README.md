[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![name status badge](https://emmottlab.r-universe.dev/badges/:name)](https://emmottlab.r-universe.dev/)
[![scpImaging status badge](https://emmottlab.r-universe.dev/scpImaging/badges/version)](https://emmottlab.r-universe.dev/scpImaging)
  [![R-CMD-check](https://github.com/emmottlab/scpImaging/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emmottlab/scpImaging/actions/workflows/R-CMD-check.yaml)



# scpImaging
Package for curation, analysis and visualisation of cellenONE-based microscopy data, for example in single-cell proteomics experiments. This package offers capabilities to integrate with cellpose and cellprofiler workflows, enabling multiplet detection and cell phenotyping.

This R package contains the R-based components of this workflow, please see the (coming soon!)`Loi, Holmes et al. (2025) bioRxiv` preprint for details downloading the pre-trained Cellpose-SAM model for segmenting cellenONE images, or drag-and-drop cellprofiler workflow for cell phenotyping.

![scpImaging Hex Sticker](https://github.com/emmottlab/scpImaging/blob/main/sticker/scpImaging_HexSticker.png)

While designed with single-cell proteomics in mind, it should be compatable with any cellenONE-based single-cell data.

## Installation
The devtools package provides `install_github()` that enables installing packages from GitHub (compiled from source):

```r
library(devtools)
install_github("emmottlab/scpImaging")
```

You can also install compiled binaries through r-universe:

```r
install.packages('scpImaging', repos = c('https://emmottlab.r-universe.dev', 'https://cloud.r-project.org'))
```


## General use cases:
This R package has functions for cropping cellenONE images based on the instrument metadata to allow for straightforward downstream processing in Cellpose. It has functions to allow generation of masked images highlighting identified cells or cell outlines.

It also has functions for calculating cell numbers isolated from Cellpose mask files (i.e. singlet/doublet/multiplet detection) and incorporating this into dataframes or QFeatures objects. These mask files can be used in Cellprofiler workflows for additional cell phenotyping which can be incorporated into Cell metadata, either at the beginning of workflows, or to singlecellexperiment or QFeatures objects.

Lastly the package includes a custom iSEE panel class, to permit visualisation of cellenONE images as part of interactive single-cell experiment data exploration using the iSEE package.

The package has been designed to work with the [`scp`](https://www.bioconductor.org/packages/release/bioc/html/scp.html) and [`iSEE`](https://bioconductor.org/packages/release/bioc/html/iSEE.html) packages and most use cases will be in conjunction with these packages. However, this package does not require downstream workflows to employ SCP, and can be employed as part of QC for any cellenONE-based workflow incorporating cell dispensing, from single-cell omics to cell-line development. 

## Vignettes
Please read: [The scpImaging vignette](https://emmottlab.github.io/scpImaging/scpImaging.html) for a overview of the package and its workflows.

## CP-SAM trained model and Cellprofiler workflow availability
Ahead of preprinting, these can be found at this dropbox link: [https://www.dropbox.com/t/rzk7W00pMCTUgXsd](https://www.dropbox.com/t/rzk7W00pMCTUgXsd)

## Feedback & asking for help
Please use [Github
issues](https://github.com/emmottlab/scpImaging/issues) or
question or report problems with `scpImaging`. 

We very much welcome feedback on scpImaging, especially while it is still in beta-testing/preprint-stage.

## Citation
To cite the `scpImaging` package in publications use:
Loi, Holmes et al. (2025) bioRxiv !coming soon! (and drop Ed an email for the most up to date citation please!), and 'this' github repository.

While separate projects, `scpImaging` is designed to work with the `scp` and `iSEE` packages. If you use these packages in your work, please refer to the relevant citations for those packages.

## License
(c) 2025, Ed Emmott, University of Liverpool, UK [emmottlab.org](https://emmottlab.org). MIT Licence.





