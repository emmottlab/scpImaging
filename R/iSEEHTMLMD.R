
######################################################################
### HTMLpanel Class Definition and Methods
######################################################################

#' The HTMLpanel Class
#'
#' The HTMLpanel class displays content from an iSEE app developer-specified Markdown or HTML file.
#' This is useful for including static documentation, for example an introduction or context for the
#' dataset.
#'
#' @section Slot overview:
#' The following slots are relevant to the content:
#' \itemize{
#' \item \code{FilePath}, a string containing the path to a Markdown or HTML file.
#' The content of this file will be rendered and displayed inside the app.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Constructor:
#' \code{HTMLpanel(...)} creates an instance of an HTMLpanel class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#' A \code{FilePath} argument \strong{must} be provided.
#'
#' @slot FilePath Character scalar specifying the path to the file (.md or.html) to be displayed.
#'
#' @author Ed Emmott
#' @seealso \linkS4class{Panel}
#' @name HTMLpanel-class
#' @aliases HTMLpanel-class
#' initialize,HTMLpanel-method
#'.defineDataInterface,HTMLpanel-method
#'.createObservers,HTMLpanel-method
#'.hideInterface,HTMLpanel-method
#'.fullName,HTMLpanel-method
#'.panelColor,HTMLpanel-method
#'.generateOutput,HTMLpanel-method
#'.renderOutput,HTMLpanel-method
#'.defineOutput,HTMLpanel-method
#'.exportOutput,HTMLpanel-method
#'.definePanelTour,HTMLpanel-method
#' @docType methods
NULL

#' @export
setClass("HTMLpanel", contains = "Panel", slots = c(FilePath = "character"))

#' @importFrom methods callNextMethod is new slot
#' @importFrom iSEE initialize
#' @export
setMethod("initialize", "HTMLpanel", function(.Object,...) {
  args <- list(...)
  if (!"FilePath" %in% names(args)) {
    stop("The 'FilePath' argument must be provided for HTMLpanel.")
  }

  # Ensure FilePath is treated as character
  args$FilePath <- as.character(args$FilePath)

  # --- Convert to absolute path and check existence ---
  absolute_path <- tryCatch({
    normalizePath(args$FilePath, mustWork = TRUE)
  }, warning = function(w) {
    stop("Error resolving FilePath '", args$FilePath, "': ", w$message, call. = FALSE)
  }, error = function(e) {
    stop("Error resolving FilePath '", args$FilePath, "': ", e$message, call. = FALSE)
  })

  # Store the absolute path back in args
  args$FilePath <- absolute_path
  # --- END ---

  # Proceed with standard initialization using the absolute path
  do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("HTMLpanel", function(object) {
  msg <- character(0)
  fp <- object@FilePath # This should now be an absolute path

  # Validate FilePath: must be a single, non-empty string
  if (length(fp)!= 1L ||!is.character(fp) || is.na(fp) || fp == "") { # Combined checks [1, 2]
    msg <- c(msg, "'FilePath' must be a single, non-missing, non-empty character string.")
  }

  # Basic check for plausible file extension [3, 4, 5, 6, 7]
  if (!grepl("\\.(md|rmd|markdown|html|htm)$", fp, ignore.case = TRUE)) {
    msg <- c(msg, "'FilePath' should likely end with.md,.rmd,.markdown,.html, or.htm")
  }

  # Existence check is now primarily handled in initialize, but a check here adds safety
  if (length(fp) == 1L && nzchar(fp) &&!is.na(fp) &&!file.exists(fp)) { # Check file existence
    msg <- c(msg, paste("File specified by 'FilePath' does not exist (or was removed after initialization):", shQuote(fp)))
  }


  if (length(msg)) {
    return(msg)
  }
  TRUE
})

#' Create an HTML Panel Instance
#'
#' This function constructs an \code{HTMLpanel} object, an iSEE panel
#' for displaying content from a Markdown or HTML file.
#'
#' @param FilePath Character scalar specifying the path to the file
#'  (.md or .html) to be displayed. This argument is mandatory.
#' @param ... Additional arguments passed to the \code{initialize} method of
#'  the parent \linkS4class{Panel} class, or to set other slots.
#'
#' @return An \code{HTMLpanel} object.
#' @author Ed Emmott
#' @seealso \code{\link{HTMLpanel-class}}
#' @export
#' @examples
#' # panel <- HTMLpanel(FilePath = "path/to/your/file.md")
HTMLpanel <- function(FilePath, ...) {
  new("HTMLpanel", FilePath = FilePath, ...)
}

#' @importFrom iSEE .fullName
#' @export
setMethod(".fullName", "HTMLpanel", function(x) "HTML panel")

#' @importFrom iSEE .panelColor
#' @export
setMethod(".panelColor", "HTMLpanel", function(x) "dodgerblue") # Example color

#' @importFrom iSEE .hideInterface
#' @export
setMethod(".hideInterface", "HTMLpanel", function(x, field) {
  if (field %in% c("RowSelectionRestrict", "ColumnSelectionRestrict",
                   "RowSelectionSource", "ColumnSelectionSource",
                   "RowSelectionDynamicSource", "ColumnSelectionDynamicSource",
                   "SelectionBoxOpen",
                   # --- NEW: Explicitly hide DataBoxOpen ---
                   "DataBoxOpen"))
  {
    return(TRUE)
  }
  callNextMethod()
})

#' @export
#' @importFrom shiny tagList
#' @importFrom iSEE .addSpecificTour .getEncodedName .defineDataInterface
setMethod(".defineDataInterface", "HTMLpanel", function(x, se, select_info) {
  # --- MODIFIED: Return an empty list to hide the Data Parameters box ---
  list()
  # --- END MODIFICATION ---
})

#' @export
#' @importFrom iSEE .createUnprotectedParameterObservers .getEncodedName .createObservers
setMethod(".createObservers", "HTMLpanel", function(x, se, input, session, pObjects, rObjects) {
  # Note: Even though the interface is hidden, we keep the observer.
  # This allows the FilePath to be potentially changed programmatically
  # (e.g., via code tracker) and have the panel react.
  # If programmatic changes are never intended, this observer could also be removed.
  callNextMethod()
  panel_name <-.getEncodedName(x)
  .createUnprotectedParameterObservers(panel_name, "FilePath", input, pObjects, rObjects)
})

#' @export
#' @importFrom shiny uiOutput hr tagList
#' @importFrom iSEE .getEncodedName .defineOutput
setMethod(".defineOutput", "HTMLpanel", function(x) {
  tagList(
    uiOutput(.getEncodedName(x)),
    hr()
  )
})

#' @export
#' @importFrom shiny renderUI HTML getDefaultReactiveDomain removeNotification
#' @importFrom iSEE .getEncodedName .retrieveOutput .renderOutput
setMethod(".renderOutput", "HTMLpanel", function(x, se,..., output, pObjects, rObjects) {
  panel_name <-.getEncodedName(x)
  output[[panel_name]] <- renderUI({
    # nocov start
    out <-.retrieveOutput(panel_name, se, pObjects, rObjects)
    HTML(out$text)
    # nocov end
  })
})

#' @export
#' @importFrom tools file_ext
#' @importFrom rmarkdown pandoc_convert
#' @importFrom shiny showNotification HTML removeNotification getDefaultReactiveDomain
#' @importFrom iSEE .getEncodedName .generateOutput
setMethod(".generateOutput", "HTMLpanel", function(x, se, all_memory, all_contents) {
  filepath <- slot(x, "FilePath")
  output_text <- NULL
  plot_name <-.getEncodedName(x) # For notification IDs

  # Ensure filepath is a single non-NA string (already validated, but why not...)
  if (length(filepath)!= 1L || is.na(filepath) || filepath == "") {
    msg <- "Internal Error: Invalid FilePath slot."
    output_text <- paste0("<p style='color:red;'>", msg, "</p>")
    showNotification(msg, type="error", duration=NULL, id=paste0(plot_name, "_internalError"))
  } else if (!file.exists(filepath)) { # Check file existence
    msg <- paste("Error: File not found at path:", shQuote(filepath))
    # nocov start
    showNotification(msg, type = "error", duration = NULL, id = paste0(plot_name, "_fileError"))
    # nocov end
    output_text <- paste0("<p style='color:red;'>", msg, "</p>")
  } else {
    # Remove previous error notifications for this panel if file now exists
    # nocov start
    removeNotification(paste0(plot_name, "_fileError"), session = shiny::getDefaultReactiveDomain())
    removeNotification(paste0(plot_name, "_renderError"), session = shiny::getDefaultReactiveDomain())
    removeNotification(paste0(plot_name, "_internalError"), session = shiny::getDefaultReactiveDomain())
    # nocov end

    ext <- tolower(tools::file_ext(filepath)) # Get file extension

    if (ext %in% c("html", "htm")) {
      result <- tryCatch({
        paste(readLines(filepath, warn = FALSE), collapse = "\n") # Read file content
      }, error = function(e) {
        # nocov start
        msg <- paste("Error reading HTML file:", shQuote(filepath), "-", e$message)
        showNotification(msg, type = "error", duration = NULL, id = paste0(plot_name, "_renderError"))
        paste0("<p style='color:red;'>", msg, "</p>")
        # nocov end
      })
      output_text <- result
    } else if (ext %in% c("md", "rmd", "markdown")) {
      tmpout <- tempfile(fileext = ".html")
      on.exit(unlink(tmpout), add = TRUE)

      render_result <- try(
        rmarkdown::pandoc_convert(input = filepath, output = tmpout, options = c("--mathjax")), # Convert markdown
        silent = TRUE
      )

      if (is(render_result, "try-error")) {
        # nocov start
        msg <- paste("Error rendering Markdown file:", shQuote(filepath), "-", as.character(render_result))
        showNotification(msg, type = "error", duration = NULL, id = paste0(plot_name, "_renderError"))
        output_text <- paste0("<p style='color:red;'>", msg, "</p>")
        # nocov end
      } else if (!file.exists(tmpout) || file.info(tmpout)$size == 0) { # Check file existence
        # nocov start
        msg <- paste("Error: Pandoc conversion failed or produced empty output for:", shQuote(filepath))
        showNotification(msg, type = "error", duration = NULL, id = paste0(plot_name, "_renderError"))
        output_text <- paste0("<p style='color:red;'>", msg, "</p>")
        # nocov end
      } else {
        # Read the successfully rendered HTML
        read_result <- tryCatch({
          paste(readLines(tmpout, warn = FALSE), collapse = "\n") # Read file content
        }, error = function(e) {
          # nocov start
          msg <- paste("Error reading temporary HTML output for:", shQuote(filepath), "-", e$message)
          showNotification(msg, type = "error", duration = NULL, id = paste0(plot_name, "_renderError"))
          paste0("<p style='color:red;'>", msg, "</p>")
          # nocov end
        })
        output_text <- read_result
      }
    } else {
      msg <- paste("Error: Unsupported file type for HTMLpanel:", shQuote(filepath), "(extension:", ext, ")")
      # nocov start
      showNotification(msg, type = "error", duration = NULL, id = paste0(plot_name, "_fileError"))
      # nocov end
      output_text <- paste0("<p style='color:red;'>", msg, "</p>")
    }
  }

  list(
    contents = NULL,
    varname = NULL,
    commands = paste0("# Content generated from file:\n# FilePath: ", shQuote(filepath)),
    text = output_text
  )
})


#' @export
#' @importFrom tools file_ext
#' @importFrom rmarkdown pandoc_convert
#' @importFrom shiny showNotification removeNotification getDefaultReactiveDomain
#' @importFrom iSEE .getEncodedName .getFullName .exportOutput
setMethod(".exportOutput", "HTMLpanel", function(x, se, all_memory, all_contents) {
  filepath <- slot(x, "FilePath")
  panel_name <-.getEncodedName(x)
  export_filename <- paste0(panel_name, ".html")
  export_path <- file.path(getwd(), export_filename) # Export to current working dir

  # Remove previous export error notification
  # nocov start
  removeNotification(paste0(panel_name, "_exportError"), session = shiny::getDefaultReactiveDomain())
  # nocov end

  if (length(filepath)!= 1L || is.na(filepath) || filepath == "" ||!file.exists(filepath)) { # Check file existence
    # nocov start
    msg <- paste("Error exporting: Source file path is invalid or file not found:", shQuote(filepath))
    showNotification(msg, type = "error", duration = NULL, id = paste0(panel_name, "_exportError"))
    write(c(msg), file = export_path)
    # nocov end
    return(export_filename)
  }

  ext <- tolower(tools::file_ext(filepath)) # Get file extension

  if (ext %in% c("html", "htm")) {
    copied <- tryCatch({
      file.copy(from = filepath, to = export_path, overwrite = TRUE) # Copy file
    }, error = function(e) {
      # nocov start
      msg <- paste("Error copying HTML file for export:", shQuote(filepath), "-", e$message)
      showNotification(msg, type = "error", duration = NULL, id = paste0(panel_name, "_exportError"))
      write(c(msg, as.character(e)), file = export_path)
      FALSE
      # nocov end
    })
    if (!copied &&!file.exists(export_path)) { # Check file existence
      # nocov start
      write("Export failed: Could not copy HTML file.", file=export_path)
      # nocov end
    }
  } else if (ext %in% c("md", "rmd", "markdown")) {
    render_result <- try(
      rmarkdown::pandoc_convert( # Convert markdown [1, 2, 7, 23, 24, 25]
        input = filepath,
        output = export_path,
        options = c("-s", "--metadata", paste0("title=",.getFullName(x)), "--mathjax") # Standalone HTML
      ),
      silent = TRUE
    )
    if (is(render_result, "try-error")) {
      # nocov start
      msg <- paste("Error rendering Markdown for export:", shQuote(filepath), "-", as.character(render_result))
      showNotification(msg, type = "error", duration = NULL, id = paste0(panel_name, "_exportError"))
      write(c(msg, as.character(render_result)), file = export_path)
      # nocov end
    } else if (!file.exists(export_path) || file.info(export_path)$size == 0) { # Check file existence
      # nocov start
      msg <- paste("Error: Pandoc export failed or produced empty output for:", shQuote(filepath))
      showNotification(msg, type = "error", duration = NULL, id = paste0(panel_name, "_exportError"))
      write(c(msg), file = export_path)
      # nocov end
    }
  } else {
    # nocov start
    msg <- paste("Error exporting: Unsupported file type", shQuote(filepath))
    showNotification(msg, type = "error", duration = NULL, id = paste0(panel_name, "_exportError"))
    write(c(msg), file = export_path)
    # nocov end
  }

  return(export_filename)
})

#' @export
#' @importFrom iSEE .getEncodedName .getPanelColor .definePanelTour
setMethod(".definePanelTour", "HTMLpanel", function(x) {
  panel_name <-.getEncodedName(x)
  out <- rbind(
    c(paste0("#", panel_name), sprintf("The <font color=\"%s\">HTMLpanel</font> displays content from a pre-defined Markdown or HTML file specified by the iSEE app developer. This allows static information, e.g. an introduction and/or statisu images to be shown alongside dynamic content from the summerisedexperiment/singlecellexperiment object.",.getPanelColor(x)))
  )

  data.frame(element = out[, 1], intro = out[, 2], stringsAsFactors = FALSE)
})
