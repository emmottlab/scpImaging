### iSEE custom panel class for cellenONE image visualization (requires iSEE)

# Status - panel functional!
# Default usage (assumes colData has "ImageFile", "Background", etc.)
#panel <- CellenONEPlot()

# Custom usage
#panel_custom <- CellenONEPlot(
#  ImageFile_Path = "MyPrimaryImageDataColumn",
#  Background_Path = "MyBackgroundDataColumn",
#  OverlayParent_Path = "MyOverlayParentFiles",
#  # ... and so on for other paths
#)


##############################################################################################################
# Helper functions for CellenONEplot
.get_filename_from_coldata <- function(sample_coldata_df,
                                       target_colname_in_coldata,
                                       all_coldata_colnames,
                                       image_type_identifier) {

  filename <- NA_character_ # Default to NA if not found or invalid

  # Check if the target column name (from the slot) exists in the provided sample's colData row
  if (target_colname_in_coldata %in% names(sample_coldata_df)) {
    retrieved_value <- sample_coldata_df[[target_colname_in_coldata]][1] # df has only one row

    # Check if the retrieved value is not NULL, not NA, and is a non-empty string
    if (!is.null(retrieved_value) && !is.na(retrieved_value) && nzchar(trimws(as.character(retrieved_value)))) {
      filename <- as.character(retrieved_value)
    } else {
      # Optional: Message if the column exists but the value is missing/NA/empty
      # message(sprintf("CellenONEPlot [Info]: Column '%s' for sample '%s' (image type '%s') has a missing or empty filename.",
      #                 target_colname_in_coldata,
      #                 rownames(sample_coldata_df)[1], # Assuming rownames has the sample ID
      #                 image_type_identifier))
    }
  } else {
    # Optional: Message if the target column name itself is not found in the colData for this sample.
    # This could indicate a misconfiguration of the panel slots or an issue with the colData structure.
    # message(sprintf("CellenONEPlot [Warning]: Column '%s' (intended for image type '%s') not found in colData for sample '%s'. Ensure panel slots like 'ImageFile_Path' match colData column names.",
    #                 target_colname_in_coldata,
    #                 image_type_identifier,
    #                 rownames(sample_coldata_df)[1]))
  }

  return(filename)
}

.generateCellenONEPlotCode <- function(panel_obj, se_object_name = "se") {
  cmds <- character(0)

  # Retrieve current slot values for configuration
  y_axis_sample_slot_name <- .cellenPlotYAxisSampName # Or its string literal
  selected_sample_val <- slot(panel_obj, y_axis_sample_slot_name)

  image_dir_slot_name <- "ImageDir" # New slot we will add
  image_dir_val <- slot(panel_obj, image_dir_slot_name)

  path_slot_names <- c("ImageFile_Path", "Background_Path", "OverlayParent_Path", "Cropped_Path", "Overlay_Path", "Mask_Path")
  image_col_names_in_coldata <- sapply(path_slot_names, function(sn) slot(panel_obj, sn))

  # Header and Library Loading
  cmds <- c(cmds, "## CellenONE Plot - Reproducible Code")
  cmds <- c(cmds, "# Generated by iSEE")
  cmds <- c(cmds, "library(SummarizedExperiment) # For colData()")
  cmds <- c(cmds, "library(ggplot2)")
  cmds <- c(cmds, "library(grid)      # For rasterGrob, unit")
  cmds <- c(cmds, "library(patchwork) # For plot layout")
  cmds <- c(cmds, "library(png)       # For readPNG")
  cmds <- c(cmds, paste0("library(scpImaging) # For helper functions like .load_and_plot_image"))

  cmds <- c(cmds, "\n## --- Configuration (derived from panel state) ---")
  cmds <- c(cmds, sprintf("selected_sample <- '%s'", selected_sample_val))
  cmds <- c(cmds, sprintf("image_directory_prefix <- '%s'", image_dir_val)) # Path to image directory

  cols_to_extract_str <- paste0("c('", paste(unique(c("EjBound", "SedBound", "CellType", image_col_names_in_coldata)), collapse="', '"),"')")
  cmds <- c(cmds, sprintf("relevant_coldata_cols <- %s", cols_to_extract_str))

  cmds <- c(cmds, "\n## --- Data Preparation ---")
  cmds <- c(cmds, sprintf("stopifnot(exists('%s')) ## Assumes your SummarizedExperiment object is named '%s'", se_object_name, se_object_name))
  cmds <- c(cmds, sprintf("coldata_full <- SummarizedExperiment::colData(%s)", se_object_name))
  cmds <- c(cmds, "if (!selected_sample %in% rownames(coldata_full)) {")
  cmds <- c(cmds, "    stop(paste('Selected sample', selected_sample, 'not found in', deparse(substitute(se_object_name))))")
  cmds <- c(cmds, "}")
  cmds <- c(cmds, sprintf(
    "df_for_plot <- as.data.frame(coldata_full[selected_sample, intersect(colnames(coldata_full), relevant_coldata_cols), drop=FALSE])"
  ))
  cmds <- c(cmds, "if (nrow(df_for_plot) == 0) stop('Failed to retrieve data for the selected sample.')")


  cmds <- c(cmds, "\n## --- Parameters for Plotting ---")
  cmds <- c(cmds, "PDCOff_val <- 180") # Matches value in .generateOutput
  cmds <- c(cmds, "EjB_val <- if ('EjBound' %in% names(df_for_plot) && !is.null(df_for_plot$EjBound) && !is.na(df_for_plot$EjBound[1])) df_for_plot$EjBound[1] + PDCOff_val else PDCOff_val")
  cmds <- c(cmds, "SdB_val <- if ('SedBound' %in% names(df_for_plot) && !is.null(df_for_plot$SedBound) && !is.na(df_for_plot$SedBound[1])) df_for_plot$SedBound[1] + EjB_val else EjB_val")
  cmds <- c(cmds, "CType_val <- if ('CellType' %in% names(df_for_plot) && !is.null(df_for_plot$CellType)) df_for_plot$CellType[1] else 'N/A'")

  cmds <- c(cmds, "\n## --- Image Filename Retrieval ---")
  image_filenames_r_vars <- paste0("img_file_", LETTERS[1:6]) # R variable names for filenames
  plot_titles <- c("ImageFile", "Background", "ImageOutline", "Cropped", "Outline", "Masks")

  for (i in seq_along(path_slot_names)) {
    actual_col_name <- image_col_names_in_coldata[i]
    cmds <- c(cmds, sprintf(
      "%s <- if ('%s' %%in%% names(df_for_plot)) df_for_plot[['%s']][1] else NA_character_",
      image_filenames_r_vars[i], actual_col_name, actual_col_name
    ))
  }

  cmds <- c(cmds, "\n## --- Generate Individual Plots ---")
  plot_r_vars <- paste0("g", LETTERS[1:6]) # gA, gB, ...
  add_vlines_flags <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)

  for(i in 1:6) {
    cmds <- c(cmds, sprintf(
      "%s <- scpImaging:::.load_and_plot_image(image_filename = %s, plot_title = '%s', placeholder_title = '%s', add_vlines = %s, PDCOff_val = PDCOff_val, EjB_val = EjB_val, SdB_val = SdB_val, image_dir_prefix = image_directory_prefix)",
      plot_r_vars[i],
      image_filenames_r_vars[i],
      plot_titles[i], # Plot title
      plot_titles[i], # Placeholder title
      add_vlines_flags[i]
    ))
  }

  cmds <- c(cmds, "\n## --- Assemble Final Plot ---")
  cmds <- c(cmds, sprintf("plot_assembly <- %s / %s / %s / (%s | %s | %s)",
                          plot_r_vars[1], plot_r_vars[2], plot_r_vars[3],
                          plot_r_vars[4], plot_r_vars[5], plot_r_vars[6]))
  cmds <- c(cmds, "plot_assembly <- plot_assembly + patchwork::plot_layout(widths = c(4,4,4,1,1,1))")
  cmds <- c(cmds, "final_plot <- plot_assembly + patchwork::plot_annotation(")
  cmds <- c(cmds, "    title = paste0('Cell: ', selected_sample),")
  cmds <- c(cmds, "    subtitle = paste0('Cell Type: ', CType_val)")
  cmds <- c(cmds, ")")
  cmds <- c(cmds, "\n## --- Render Plot ---")
  cmds <- c(cmds, "print(final_plot)")

  return(list(plot = cmds))
}

# Helper function to create a placeholder plot
.create_placeholder_plot <- function(title_text) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = title_text, size = 5, hjust = 0.5) +
    ggtitle(title_text) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}

.load_and_plot_image <- function(image_filename, plot_title, placeholder_title,
                                 add_vlines = TRUE, PDCOff_val, EjB_val, SdB_val,
                                 image_dir_prefix = "www") { # User should make this absolute for robust export
  image_file_path <- if (!is.na(image_filename) && nzchar(image_filename)) {
    file.path(image_dir_prefix, image_filename)
  } else {
    NA_character_
  }
  img_data <- NULL

  if (!is.na(image_file_path) && file.exists(image_file_path)) {
    img_data <- tryCatch({
      png::readPNG(image_file_path)
    }, error = function(e) {
      # message(sprintf("CellenONEPlot [Info]: Failed to read image '%s': %s", image_file_path, e$message))
      return(NULL)
    })
  }
  # If file_actually_exists is FALSE, or readPNG failed, img_data will be NULL.

  if (!is.null(img_data)) {
    img_dims <- dim(img_data)
    if (length(img_dims) < 2 || any(img_dims[1:2] == 0)) {
      return(.create_placeholder_plot(placeholder_title))
    }
    g_raster <- grid::rasterGrob(img_data, width = unit(1, "npc"), height = unit(1, "npc"))
    p <- ggplot() +
      annotation_custom(g_raster, xmin = 0, xmax = img_dims[2], ymin = 0, ymax = img_dims[1]) +
      xlim(c(0, img_dims[2])) + ylim(c(0, img_dims[1])) +
      coord_fixed(ratio = 1) + ggtitle(plot_title) + theme_void()
    if (add_vlines) {
      p <- p +
        geom_vline(xintercept = PDCOff_val, color = 'yellow', linewidth = 1) +
        geom_vline(xintercept = EjB_val, color = 'red', linewidth = 1) +
        geom_vline(xintercept = SdB_val, color = 'green', linewidth = 1)
    }
    return(p)
  } else {
    return(.create_placeholder_plot(placeholder_title)) # Assuming .create_placeholder_plot is globally available
  }
}



#' The CellenONEPlot Class
#'
#' The CellenONEPlot class displays CellenONE and scpImaging pipeline-derived micrographs dynamically,
#' as relevant entries in a single-cell experiment are selected through an iSEE app.
#'
#' @section Slot overview:
#' The following slots control the data retrieval and display:
#' \itemize{
#' \item \code{ImageFile_Path}, character. The column name in \code{colData(se)} for parent image filenames. Default: \code{"ImageFile"}.
#' \item \code{Background_Path}, character. The column name in \code{colData(se)} for background image filenames. Default: \code{"Background"}.
#' \item \code{OverlayParent_Path}, character. The column name in \code{colData(se)} for overlayed parent image filenames. Default: \code{"CP_Overlay_Parent"}.
#' \item \code{Cropped_Path}, character. The column name in \code{colData(se)} for cropped image filenames. Default: \code{"Cropped"}.
#' \item \code{Overlay_Path}, character. The column name in \code{colData(se)} for cropped and overlayed image filenames. Default: \code{"CP_Overlay"}.
#' \item \code{Mask_Path}, character. The column name in \code{colData(se)} for mask filenames. Default: \code{"CP_Mask"}.
#' \item \code{ImageDir}, character. The directory path prefixed to all image filenames. Default: \code{"www"}.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Constructor:
#' \code{CellenONEPlot(...)} creates an instance of a CellenONEPlot class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#' @author Ed Emmott
#' @seealso \linkS4class{SampleAssayPlot}
#' @name CellenONEPlot-class
#' @aliases CellenONEPlot-class
#' initialize,CellenONEPlot-method
#'.defineDataInterface,CellenONEPlot-method
#'.hideInterface,CellenONEPlot-method
#'.fullName,CellenONEPlot-method
#'.panelColor,CellenONEPlot-method
#'.generateOutput,CellenONEPlot-method
#'.renderOutput,CellenONEPlot-method
#'.defineOutput,CellenONEPlot-method
#'.exportOutput,CellenONEPlot-method
#'.definePanelTour,CellenONEPlot-method
#' @docType methods
NULL


.cellenPlotYAxisSampName <- "YAxisSampleName"
.cellenPlotYAxisColTable <- "YAxisSampleSource"
.cellenPlotYAxisSampDynamic <- "YAxisSampleDynamicSource"

#' @slot ImageFile_Path character. Specifies the column name in `colData(se)` that contains the path to the primary image file.
#' @slot Background_Path character. Specifies the column name in `colData(se)` that contains the path to the background image file.
#' @slot OverlayParent_Path character. Specifies the column name in `colData(se)` for overlay parent image filenames.
#' @slot Cropped_Path character. Specifies the column name in `colData(se)` for cropped image filenames.
#' @slot Overlay_Path character. Specifies the column name in `colData(se)` for cropped overlay image filenames.
#' @slot Mask_Path character. Specifies the column name in `colData(se)` for mask image filenames.
#' @slot ImageDir character. Specifies the directory prefix for all image files (e.g., "www").
#' @export
#' @importClassesFrom iSEE SampleAssayPlot
#' @importFrom iSEE .defineDataInterface .fullName .panelColor .refineParameters
#' @importFrom iSEE .singleSelectionSlots .hideInterface .defineOutput .generateOutput
#' @importFrom iSEE .renderOutput .exportOutput .definePanelTour
setClass("CellenONEPlot", contains='SampleAssayPlot',
         slots=c(ImageFile_Path="character",
                 Background_Path="character",
                 OverlayParent_Path="character",
                 Cropped_Path="character",
                 Overlay_Path="character",
                 Mask_Path="character",
                 ImageDir="character"
         ))

#' @importFrom methods callNextMethod is new slot
#' @importFrom iSEE .emptyDefault .noSelection getPanelDefault
#' @export
setMethod("initialize", "CellenONEPlot", function(.Object, ...) {
  args <- list(...)
  args <- .emptyDefault(args, .cellenPlotYAxisColTable, .noSelection)
  args <- .emptyDefault(args, .cellenPlotYAxisSampName, NA_character_)
  args <- .emptyDefault(args, .cellenPlotYAxisSampDynamic, getPanelDefault("SingleSelectionDynamicSource"))

  # Default column names in colData(se)
  args <- .emptyDefault(args, "ImageFile_Path", "ImageFile")
  args <- .emptyDefault(args, "Background_Path", "Background")
  args <- .emptyDefault(args, "OverlayParent_Path", "CP_Overlay_Parent")
  args <- .emptyDefault(args, "Cropped_Path", "Cropped")
  args <- .emptyDefault(args, "Overlay_Path", "CP_Overlay")
  args <- .emptyDefault(args, "Mask_Path", "CP_Mask")
  args <- .emptyDefault(args, "ImageDir", "www") # Default for new slot
  do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
#' @importFrom iSEE .validStringError .singleStringError
setValidity2("CellenONEPlot", function(object) {
  msg <- character(0)

  msg <- .validStringError(msg, object, "ImageFile_Path")
  msg <- .validStringError(msg, object, "Background_Path")
  msg <- .validStringError(msg, object, "OverlayParent_Path") # New
  msg <- .validStringError(msg, object, "Cropped_Path")       # New
  msg <- .validStringError(msg, object, "Overlay_Path")       # New
  msg <- .validStringError(msg, object, "Mask_Path")          # New
  msg <- .validStringError(msg, object, "ImageDir")

  msg <- .singleStringError(msg, object,
                            c(.cellenPlotYAxisColTable, .cellenPlotYAxisSampName))

  if (length(msg)){
    return(msg)
  }
  TRUE
})


#' Create an Instance of a CellenONEPlot Panel
#'
#' This function is the constructor for creating \code{\link{CellenONEPlot-class}} objects,
#' which represent an iSEE panel for visualizing CellenONE microscopy images.
#'
#' @param ... Arguments passed to the \code{initialize} method of the
#'   \code{\link{CellenONEPlot-class}}. These can include initial values for
#'   any of the slots defined in \code{CellenONEPlot-class} or its parent classes,
#'   such as \code{ImageFile_Path}, \code{Background_Path}, \code{ImageDir}, etc.
#'
#' @return A \code{CellenONEPlot} object.
#' @export
#' @author Ed Emmott
#' @seealso \code{\link{CellenONEPlot-class}} for details on the class slots and methods.
#' @examples
#' # Basic constructor
#' # CellenONEPlot()
#'
#' # Constructor with custom slot values (if 'se' is a SummarizedExperiment)
#' # CellenONEPlot(ImageDir = "my_image_directory/", YAxisSampleName = colnames(se)[1])
CellenONEPlot <- function(...) {
  new("CellenONEPlot", ...)
}

#' @importFrom iSEE .fullName
#' @export
setMethod(".fullName", "CellenONEPlot", function(x) "CellenONE Plot")

#' @importFrom iSEE .panelColor
#' @export
setMethod(".panelColor", "CellenONEPlot", function(x) "#fe6799") # Scienion 'hot pink'

.choose_link <- function(chosen, available) {
  if (!chosen %in% available) {
    if (length(available)) {
      return(available[1])
    }
    return("")
  }
  return(chosen)
}

#' @export
#' @importFrom methods callNextMethod
#' @importFrom iSEE .replaceMissingWithFirst .refineParameters
#' @describeIn CellenONEPlot-class Method to refine parameters after initialization or upon changes
#' to the \code{SummarizedExperiment} object. It ensures that a valid sample name is selected
#' from the available samples in \code{colData(se)}.
#' @param x A \code{CellenONEPlot} object.
#' @param se A \code{SummarizedExperiment} object containing the data.
#' @return A modified \code{CellenONEPlot} object with updated parameters, or \code{NULL} if
#' refinement is not possible (e.g., no samples in \code{se}).
setMethod(".refineParameters", "CellenONEPlot", function(x, se) {
  x <- callNextMethod()
  if (is.null(x)) {
    return(NULL)
  }
  if (ncol(se)==0L) {
    warning(sprintf("no columns for plotting '%s'", class(x)[1]))
    return(NULL)
  }

  x <- .replaceMissingWithFirst(x, .cellenPlotYAxisSampName, colnames(se))

  x
})


#' @importFrom iSEE .getEncodedName .selectizeInput.iSEE
#' @importFrom shiny selectInput checkboxInput
#' @importFrom methods slot
#' @export
setMethod(".defineDataInterface", "CellenONEPlot", function(x, se, select_info) {
  panel_name <- .getEncodedName(x)
  .input_FUN <- function(field) { paste0(panel_name, "_", field) }
  tab_by_col <- select_info$single$sample

  ui_elements <- list(
    .selectizeInput.iSEE(
      x, .cellenPlotYAxisSampName,
      label="Sample of interest (Y-axis):",
      choices=NULL, selected=NULL, multiple=FALSE),
    selectInput(
      .input_FUN(.cellenPlotYAxisColTable), label=NULL, choices=tab_by_col,
      selected=.choose_link(slot(x, .cellenPlotYAxisColTable), tab_by_col)),
    checkboxInput(
      .input_FUN(.cellenPlotYAxisSampDynamic),
      label="Use dynamic sample selection for the y-axis",
      value=slot(x, .cellenPlotYAxisSampDynamic))
  )

  ui_elements
})

#' @export
#' @importFrom methods callNextMethod
#' @importFrom iSEE .singleSelectionSlots
#' @describeIn CellenONEPlot-class Method to define the slots involved in single selections for this panel.
#' This allows the panel to react to or transmit single selections (e.g., selected sample name for the Y-axis).
#' @param x A \code{CellenONEPlot} object.
#' @return A list specifying the single selection slots, adding the Y-axis sample selection
#' configuration to those inherited from the parent class.
setMethod(".singleSelectionSlots", "CellenONEPlot", function(x) {
  c(callNextMethod(),
    list(
      list(
        parameter=.cellenPlotYAxisSampName,
        source=.cellenPlotYAxisColTable,
        dimension="sample",
        dynamic=.cellenPlotYAxisSampDynamic,
        use_mode=NA,
        use_value=NA,
        protected=TRUE
      )
    )
  )
})

#' @importFrom methods callNextMethod
#' @importFrom iSEE .hideInterface
#' @export
setMethod(".hideInterface", "CellenONEPlot", function(x, field) {
  if (field %in% c(
    "RowSelectionSource",
    "RowSelectionType",
    "RowSelectionRestrict",
    "RowSelectionDynamicSource",
    "SelectionHistory",
    "SelectionBoxOpen",
    "VisualBoxOpen" #
  )) {
    TRUE # Return TRUE to hide these elements
  } else {
    # For any other field, defer to the parent class's decision.
    callNextMethod()
  }
})

#' @importFrom iSEE .getEncodedName .panelColor
#' @importFrom shiny plotOutput
#' @importFrom shinyWidgets addSpinner
#' @importFrom methods slot
#' @export
setMethod(".defineOutput", "CellenONEPlot", function(x) {
  plot_name <- .getEncodedName(x)

  addSpinner(
    plotOutput(plot_name, height = paste0(slot(x, "PanelHeight"), "px")),
    color=.panelColor(x))
})
#' @importFrom SummarizedExperiment colData
#' @importFrom ggplot2 ggplot annotate ggtitle coord_fixed theme_void theme element_text element_rect annotation_custom xlim ylim geom_vline
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom grid rasterGrob unit
#' @importFrom png readPNG
#' @importFrom methods slot
#' @export
setMethod(".generateOutput", "CellenONEPlot",
          function(x, se, all_memory, all_contents) {

            samp_selected <- slot(x, .cellenPlotYAxisSampName)
            if (is.null(samp_selected) || is.na(samp_selected) || length(samp_selected) == 0) {
              return(list(plot = .create_placeholder_plot("No sample selected"), commands = NULL, varname = NULL, contents = NULL))
            }

            coldata_se <- colData(se)
            se_col_data_names <- colnames(coldata_se)
            df <- as.data.frame(coldata_se[rownames(coldata_se) %in% samp_selected, , drop=FALSE])

            if (nrow(df) == 0) {
              # This message is informative if a selected sample disappears, keep it if desired.
              # message(sprintf("CellenONEPlot: Sample '%s' not found in colData.", samp_selected))
              return(list(plot = .create_placeholder_plot(paste("Sample", samp_selected, "not found")), commands = NULL, varname = NULL, contents = NULL))
            }

            PDCOff <- 180
            EjB_from_df <- NA_real_
            SdB_from_df <- NA_real_

            if (nrow(df) > 0) {
              if ("EjBound" %in% names(df)) {
                val_ej <- df$EjBound[1]
                if (!is.null(val_ej)) {
                  if (is.na(val_ej)) { EjB_from_df <- NA_real_ } else {
                    converted_val_ej <- suppressWarnings(as.numeric(as.character(val_ej)))
                    if (!is.na(converted_val_ej)) { EjB_from_df <- converted_val_ej }
                    # else { message(sprintf("CellenONEPlot [Info]: 'EjBound' for sample '%s' ('%s') not valid number.", samp_selected, val_ej)) }
                  }
                } # else { message(sprintf("CellenONEPlot [Info]: 'EjBound' for sample '%s' is NULL.", samp_selected)) }
              } # else { message(sprintf("CellenONEPlot [Info]: Column 'EjBound' not found for sample '%s'.", samp_selected)) }
              EjB <- if (!is.na(EjB_from_df)) EjB_from_df + PDCOff else PDCOff

              if ("SedBound" %in% names(df)) {
                val_sd <- df$SedBound[1]
                if (!is.null(val_sd)) {
                  if (is.na(val_sd)) { SdB_from_df <- NA_real_ } else {
                    converted_val_sd <- suppressWarnings(as.numeric(as.character(val_sd)))
                    if (!is.na(converted_val_sd)) { SdB_from_df <- converted_val_sd }
                    # else { message(sprintf("CellenONEPlot [Info]: 'SedBound' for sample '%s' ('%s') not valid number.", samp_selected, val_sd)) }
                  }
                } # else { message(sprintf("CellenONEPlot [Info]: 'SedBound' for sample '%s' is NULL.", samp_selected)) }
              } # else { message(sprintf("CellenONEPlot [Info]: Column 'SedBound' not found for sample '%s'.", samp_selected)) }
              SdB <- if (!is.na(SdB_from_df)) SdB_from_df + EjB else EjB
              CType <- if ("CellType" %in% names(df) && !is.null(df$CellType)) df$CellType[1] else "N/A"
            } else {
              EjB <- PDCOff; SdB <- PDCOff; CType <- "N/A"
            }

            current_image_dir <- slot(x, "ImageDir")

            image_filename_A <- .get_filename_from_coldata(df, slot(x, "ImageFile_Path"), se_col_data_names, "ImageFile")
            image_filename_B <- .get_filename_from_coldata(df, slot(x, "Background_Path"), se_col_data_names, "Background")
            image_filename_C <- .get_filename_from_coldata(df, slot(x, "OverlayParent_Path"), se_col_data_names, "ImageOutline")
            image_filename_D <- .get_filename_from_coldata(df, slot(x, "Cropped_Path"), se_col_data_names, "Cropped")
            image_filename_E <- .get_filename_from_coldata(df, slot(x, "Overlay_Path"), se_col_data_names, "Outline")
            image_filename_F <- .get_filename_from_coldata(df, slot(x, "Mask_Path"), se_col_data_names, "Masks")

            gA <- .load_and_plot_image(image_filename_A, "ImageFile", "ImageFile", add_vlines = TRUE, PDCOff_val = PDCOff, EjB_val = EjB, SdB_val = SdB, image_dir_prefix = current_image_dir)
            gB <- .load_and_plot_image(image_filename_B, "Background", "Background", add_vlines = TRUE, PDCOff_val = PDCOff, EjB_val = EjB, SdB_val = SdB, image_dir_prefix = current_image_dir)
            gC <- .load_and_plot_image(image_filename_C, "ImageOutline", "ImageOutline", add_vlines = TRUE, PDCOff_val = PDCOff, EjB_val = EjB, SdB_val = SdB, image_dir_prefix = current_image_dir)
            gD <- .load_and_plot_image(image_filename_D, "Cropped", "Cropped", add_vlines = FALSE, PDCOff_val = PDCOff, EjB_val = EjB, SdB_val = SdB, image_dir_prefix = current_image_dir)
            gE <- .load_and_plot_image(image_filename_E, "Outline", "Outline", add_vlines = FALSE, PDCOff_val = PDCOff, EjB_val = EjB, SdB_val = SdB, image_dir_prefix = current_image_dir)
            gF <- .load_and_plot_image(image_filename_F, "Masks", "Masks", add_vlines = FALSE, PDCOff_val = PDCOff, EjB_val = EjB, SdB_val = SdB, image_dir_prefix = current_image_dir)

            patch <- gA / gB / gC / (gD | gE | gF)
            patch <- patch + plot_layout(widths = c(4,4,4,1,1,1))

            plot_out <- patch + plot_annotation(
              title = paste0("Cell: ", samp_selected),
              subtitle = paste0("Cell Type: ", CType)
            )

            cmds_list <- .generateCellenONEPlotCode(x, se_object_name = "--SE_NAME--") # Using iSEE placeholder

            return(list(commands = cmds_list, plot = plot_out, varname = NULL, contents = NULL))
          }
)

#' @importFrom shiny renderPlot
#' @importFrom iSEE .getEncodedName .retrieveOutput
#' @importFrom methods callNextMethod
#' @export
setMethod(".renderOutput", "CellenONEPlot",
          function(x, se, output, pObjects, rObjects) {

            plot_name <- .getEncodedName(x)
            force(se)

            output[[plot_name]] <- renderPlot({
              .retrieveOutput(plot_name, se, pObjects, rObjects)
            })

            callNextMethod()
          })

#' @importFrom iSEE .generateOutput .getEncodedName
#' @importFrom grDevices pdf dev.off
#' @importFrom methods slot
#' @export
setMethod(".exportOutput", "CellenONEPlot",
          function(x, se, all_memory, all_contents) {

            contents <- .generateOutput(x, se, all_memory=all_memory,
                                        all_contents=all_contents)

            newpath <- paste0(.getEncodedName(x), ".pdf")

            pdf(newpath, width=slot(x, "PanelHeight") / 75,
                height=slot(x, "PanelWidth") * 2)

            print(contents$plot)
            dev.off()

            newpath
          })


#' @importFrom methods callNextMethod
#' @importFrom iSEE .getEncodedName .fullName .addTourStep .dataParamBoxOpen
#' @importFrom methods slot
#' @describeIn CellenONEPlot Defines the interactive tour for this panel.
#' @export
setMethod(".definePanelTour", "CellenONEPlot", function(x) {
  panel_name <- .getEncodedName(x)
  fullName <- .fullName(x)

  # Slot name string values from your constants
  y_axis_sample_name_slot_str <- .cellenPlotYAxisSampName
  y_axis_col_table_slot_str <- .cellenPlotYAxisColTable
  y_axis_samp_dynamic_slot_str <- .cellenPlotYAxisSampDynamic

  # Build the tour steps as a list of data.frames, then rbind at the end
  all_steps_list <- list()

  # Step 1: Introduction - Target the main plot area
  all_steps_list[[length(all_steps_list) + 1]] <- data.frame(
    element = sprintf("#%s", panel_name), # Targets the plotOutput div
    intro = paste0(
      "Welcome to the <strong>", fullName, "</strong>!<br>",
      "This main area (highlighted) displays various microscopy images, e.g. full, cropped etc for the selected cell. ",
      "Use the 'Data parameters' box (introduced next) to choose a sample and configure other settings."
    ),
    stringsAsFactors = FALSE
  )

  # Step 2: Introduce the Data Parameters collapsible box
  all_steps_list[[length(all_steps_list) + 1]] <- iSEE:::.addTourStep(
    x,
    iSEE:::.dataParamBoxOpen, # This constant holds "DataBoxOpen"
    text = paste0(
      "The <strong>Data parameters</strong> box contains settings to select the sample. ",
      "<strong>Action:</strong> Click this bar to expand the options. ",
      "After expanding, click 'Next' on this tour pop-up to see details about the controls inside."
    )
  )

  # Step 3: 'Sample of interest' (YAxisSampleName)
  all_steps_list[[length(all_steps_list) + 1]] <- data.frame(
    element = sprintf("#%s_%s + div.selectize-control", panel_name, y_axis_sample_name_slot_str),
    intro = paste0(
      "<strong>Sample of Interest:</strong><br>",
      "Select the specific cell or sample of interest from this list. The images displayed will update accordingly."
    ),
    stringsAsFactors = FALSE
  )

  # Step 4: 'Sample Selection Source' (YAxisSampleColTable)
  all_steps_list[[length(all_steps_list) + 1]] <- data.frame(
    element = sprintf("#%s_%s + div.selectize-control", panel_name, y_axis_col_table_slot_str),
    intro = paste0(
      "<strong>Sample Selection Source:</strong><br>",
      "To dynamically update the sample based on selections in other panels (like a table or another plot), choose the source panel here. ",
      "Requires 'Use Dynamic Sample Selection' to be checked."
    ),
    stringsAsFactors = FALSE
  )

  # Step 5: 'Use Dynamic Sample Selection' checkbox (YAxisSampleDynamicSource)
  all_steps_list[[length(all_steps_list) + 1]] <- data.frame(
    element = sprintf("#%s_%s", panel_name, y_axis_samp_dynamic_slot_str), # Targets checkbox input
    intro = paste0(
      "<strong>Use Dynamic Sample Selection:</strong><br>",
      "Check this box to enable the sample displayed to change automatically when a linked 'Sample Selection Source' panel makes a new selection. ",
      "Uncheck to keep the currently selected 'Sample of Interest' static."
    ),
    stringsAsFactors = FALSE
  )

  final_tour_steps <- do.call(rbind, all_steps_list)

  return(final_tour_steps)
})
