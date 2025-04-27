# scpImaging
Package for curation, analysis and visualisation of cellenONE-based microscopy data, for example in single-cell proteomics experiments.

# Installation
The devtools package provides `install_github()` that enables installing packages from GitHub.

```r
library(devtools)
install_github("emmottlab/scpImaging")
```

# General use cases:
This R package has functions for cropping cellenONE images based on the instrument metadata to allow for straightforward downstream processing in cellpose. It has functions to allow generation of masked images highlighting identified cells or cell outlines.

It also has functions for calculating cell numbers isolated from cellpose mask files (i.e. singlet/doublet/multiplet detection) and incorporating this into dataframes of QFeatures objects. 

Lastly the package includes a custom iSEE panel class, to permit visualisation of cellenONE images as part of interactive single-cell experiment data exploration using the iSEE package.

# Vignettes
On their way...

# License
(c) 2025, Ed Emmott, University of Liverpool, UK emmottlab.org. MIT Licence.


