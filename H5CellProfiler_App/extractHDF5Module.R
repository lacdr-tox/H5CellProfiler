# Extract HDF5 Shiny Module
source('utils.R')

### GLOBAL STUFF ###

selected_h5 <- NULL
default_layout_option <- c("Select a layout files" = "")

### HDF5 UI ####

extractHDF5UI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Input files"),
    helpText("Here you can set the HDF5 files and the layout file you want to use."),
    bsAlert(ns("input_dir_set_alert")),
    uiOutput(ns("hdf5s")),
    bsAlert(ns("no_hdf5s_in_dir")),
    shiny::tags$label("Layout file:"),
    bsAlert(ns("layout_file_info")),
    bsAlert(ns("no_tsv_in_dir")),
    selectInput(ns("tsvfile"), label = NULL, choices = default_layout_option),
    bsAlert(ns("layout_reading_alert")),
    uiOutput(ns("preview_layout")),
    h4("Results"),
    helpText("It is possible to show the information extracted from the HDF5 files in the browser.
             In this way you can get an overview of your data, and do some preliminary analysis.
             You can also export data and plots."),
    checkboxInput(ns("show_results"), "Show results in browser"),
    uiOutput(ns("test"))

  )
}

### HDF5 SERVER ###

extractHDF5 <- function(input, output, session, inputDirectory) {
  ns <- session$ns
  createAlert(session, ns("layout_file_info"), "layout_file_info",
              content = HTML("Layout files are Tab Separated Value (TSV) files saved as
                  <code>.tsv</code>, <code>.tab</code>, or <code>.txt</code>."),
              style = "info", dismiss = FALSE)

  observe({
    if(is.null(inputDirectory())) {
    createAlert(session, ns("input_dir_set_alert"), "input_dir_set_alert",
                content = HTML("<strong>Attention</strong> Please set an input directory in the left panel"),
                style = "danger", dismiss = FALSE)
    } else {
      closeAlert(session, "input_dir_set_alert")
    }
  })

  # Get list of h5 files from the input directory. We use a reactivePoll because with
  # invalidateLater the UI will refresh even when the directory contents do not change.
  dirH5Files <- function() {
    if(!is.null(inputDirectory())) dir(inputDirectory(), "*.h5") else NULL
  }
  h5files_in_inputdir <- reactivePoll(1000, session, dirH5Files, dirH5Files)

  # Update the choices and alert for hdf5s
  observe({
    if(length(h5files_in_inputdir()) == 0) {
      createAlert(session, ns("no_hdf5s_in_dir"), "no_hdf5s_in_dir",
                  content = HTML("<strong>Warning</strong> No HDF5 files found"),
                  style = "warning", dismiss = FALSE)
    } else {
      closeAlert(session, "no_hdf5s_in_dir")
    }
  })

  # TODO replace this with a updateCheckboxGroupInput, but impossible due to
  # https://github.com/rstudio/shiny/issues/1144
  output$hdf5s <- renderUI({
    choices <- h5files_in_inputdir()
    selected <- intersect(choices, selected_h5)
    checkboxGroupInput(ns("h5files"), label = "HDF5 files:", choices = choices, selected = selected)
  })
  # Store selected HDF5 in global variable to survive renderUI refresh
  observe({selected_h5 <<- input$h5files})

  # Possible layout files should be in tsv format
  dirTsvFiles <- function() {
    if(!is.null(inputDirectory())) dir(inputDirectory(), "*.(tsv|tab|txt)") else NULL
  }
  tsvfiles_in_inputdir <- reactivePoll(1000, session, dirTsvFiles, dirTsvFiles)

  # Update the choices for layout files
  observe({
    if(length(tsvfiles_in_inputdir()) == 0) {
      createAlert(session, ns("no_tsv_in_dir"), "no_tsv_in_dir",
                  content = HTML("<strong>Warning</strong> No possible layout files found"),
                  style = "warning", dismiss = FALSE)
    } else {
      closeAlert(session, "no_tsv_in_dir")
    }
    # isolate this because we don't want a dependency on input$tsvfile
    isolate(
      updateSelectInput(session, "tsvfile",
                        choices = c(default_layout_option, tsvfiles_in_inputdir()),
                        selected = input$tsvfile)
    )
  })

  extractHDF5_input_config <- reactive({
    list(
      "hdf5" = input$h5files,
      "layout" = input$tsvfile
    )
  })

  layout_file <- reactive({
    if (!is.null(input$tsvfile) && nchar(input$tsvfile) > 0) {
      return(file.path(inputDirectory(), input$tsvfile))
    }
    return(NULL)
  })

  layout <- reactive({
    if (is.null(layout_file())) {
      return(NULL)
    }
    layout <- tryCatch({
      data <- read.table(layout_file(), sep = "\t", header = TRUE, comment.char = "")
      # If we're here, we could read the successfully, so we can close the alert
      closeAlert(session, "layout_reading_alert")
      return(data)
    }, error = function(e) {
      # Error reading layout file, create warning
      createAlert(session, ns("layout_reading_alert"), "layout_reading_alert",
                  content = HTML("<strong>Error</strong> Could not read layout file, is it in the
                                 correct format?"), style = "danger", dismiss = FALSE)
      return(NULL)
    })
  })

  output$preview_layout <- renderUI({
    if(is.null(layout())) {return(NULL)}
    dataTableOutput(ns("layout_dt"))
  })

  output$layout_dt <- renderDataTable(getOrElse(layout(),data.frame()), options = list(pageLength = 5,
                                                               lengthMenu = c(5, 10, 15, 20)))

  extractHDF5Config <- reactive({
    list(
      "input" = extractHDF5_input_config(),
      "show-results" = input$show_results
    )
  })

  return(extractHDF5Config)
}
