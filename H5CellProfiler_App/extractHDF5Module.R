# Extract HDF5 Shiny Module
source('utils.R')
source('../utils/cphdf5.R')

### GLOBAL STUFF ###

selected_h5 <- NULL
default_layout_option <- c("Select a layout file" = "")
default_location_id_option <- c("Select location ID" = "")
default_plate_id_option <- c("Select plate ID" = "")
default_image_id_option <- c("Select image ID" = "")
default_time_id_option <- c("Select time ID" = "")
default_replicate_id_option <- c("Select replicate ID" = "")
default_prim_obj_option <- c("Select primary object" = "")
default_sec_obj_option <- c("Select secondary objects" = "")
default_tert_obj_option <- c("Select tertiary object" = "")
default_features_option <- c("Select features" = "")

full_width <- validateCssUnit("100%")

### HDF5 UI ####

extractHDF5UI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Input files"),
    helpText("Here you can set the HDF5 files and the layout file you want to use."),
    bsAlert(ns("input_dir_set_alert")),
    uiOutput(ns("hdf5s")),
    bsAlert(ns("no_hdf5s_in_dir")),
    bsAlert(ns("h5_reading_alert")),
    shiny::tags$label("Layout file:"),
    bsAlert(ns("layout_file_info")),
    bsAlert(ns("no_tsv_in_dir")),
    selectInput(ns("tsvfile"), label = NULL, choices = default_layout_option),
    bsAlert(ns("layout_reading_alert")),
    uiOutput(ns("preview_layout")),
    h4("Metadata"),
    helpText(HTML("Here you can set the metadata. The metadata options are extracted from the
                  <strong>first</strong> HDF5 file and the layout file.")),
    helpText("HDF5 metadata that corresponds to locationID in layout file (usually wells)."),
    selectInput(ns("location_id"), label = "Location ID", choices = default_location_id_option),
    helpText("Plate ID is used to couple metadata from the layout file. Can be
             HDF5 metadata or plateID from the layout file."),
    selectInput(ns("plate_id"), label = "Plate ID", choices = default_plate_id_option),
    helpText("I don't know what this is for, usually you can pick the same as for Location ID."),
    selectInput(ns("image_id"), label = "Image ID", choices = default_image_id_option),
    helpText("Time ID is to get the frame numbers from the images. Currently
             only HDF5 metadata is supported."),
    selectInput(ns("time_id"), label = "Time ID", choices = default_time_id_option),
    helpText("(Optional) Replicate ID is used for plotting options."),
    selectInput(ns("replicate_id"), label = "Replicate ID", choices = default_replicate_id_option),
    timeInput(ns("exposure_delay"), label = "Exposure delay", seconds = FALSE),
    timeInput(ns("time_between_frame"), label = "Time between frames"),
    h4("Objects"),
    selectInput(ns("primary_object"), label = "Primary (parent) object",
                choices = default_prim_obj_option),
    selectInput(ns("secondary_object"), label = "Secondary (child) objects",
                choices = default_sec_obj_option, multiple = TRUE),
    selectInput(ns("tertiary_object"), label = "Tertiary object",
                choices = default_tert_obj_option),
    h4("Features"),
    helpText("Object features to report. If you can't see any features, please
             make sure you have selected some objects."),
    selectInput(ns("features"), label = "Features", choices = default_features_option,
                multiple = TRUE, width = full_width),
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

  observeEvent(h5_metadata(), {
    isolate(
      updateSelectInput(session, "location_id",
                        choices = c(default_location_id_option, h5_metadata()),
                        selected = input$location_id)
    )
  })

  observeEvent(c(h5_metadata(), plate_ids_from_layout()), {
    isolate(
      updateSelectInput(session, "plate_id",
                        choices = c(default_plate_id_option, plate_ids_from_layout(), h5_metadata()),
                        selected = input$plate_id)
    )
  })

  observeEvent(h5_metadata(), {
    isolate(
      updateSelectInput(session, "image_id",
                        choices = c(default_image_id_option, h5_metadata()),
                        selected = input$image_id)
    )
  })

  observeEvent(h5_metadata(), {
    isolate(
      updateSelectInput(session, "time_id",
                        choices = c(default_time_id_option, h5_metadata()),
                        selected = input$time_id)
    )
  })

  observeEvent(h5_metadata(), {
    isolate(
      updateSelectInput(session, "replicate_id",
                        choices = c(default_replicate_id_option, h5_metadata()),
                        selected = input$replicate_id)
    )
  })

  observeEvent(h5_objects(), {
    isolate(
      updateSelectInput(session, "primary_object",
                        choices = c(default_prim_obj_option, h5_objects()),
                        selected = input$primary_object)
    )
  })

  observeEvent(h5_objects(), {
    isolate(
      updateSelectInput(session, "secondary_object",
                        choices = c(default_sec_obj_option, h5_objects()),
                        selected = input$secondary_object)
    )
  })

  observeEvent(h5_objects(), {
    isolate(
      updateSelectInput(session, "tertiary_object",
                        choices = c(default_tert_obj_option, h5_objects()),
                        selected = input$tertiary_object)
    )
  })

  observeEvent(h5_features(), {
    isolate(
      updateSelectInput(session, "features",
                        choices = c(default_features_option, h5_features()),
                        selected = input$features)
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

  first_h5_name <- reactive({
    if(!is.null(input$h5files)) input$h5files[1] else NULL
  })

  first_h5_file <- reactive({
    if(!is.null(inputDirectory()) && !is.null(first_h5_name())) {
      return(file.path(inputDirectory(), first_h5_name()))
    }
    return(NULL)
  })

  h5_measurements <- reactive({
    if (is.null(first_h5_file())) {
      closeAlert(session, "h5_reading_alert")
      return(NULL)
    }
    h5_measurements <- tryCatch({
      measurements <- getMeasurementsFromCPH5(first_h5_file())
      # If we're here, we could read the successfully, so we can close the alert
      closeAlert(session, "h5_reading_alert")
      return(measurements)
    }, error = function(e) {
      # Error reading layout file, create warning
      createAlert(session, ns("h5_reading_alert"), "h5_reading_alert",
                  content = HTML(paste("<strong>Error</strong> Could not read <code>", first_h5_name(),
                                       "</code>, is it a CellProfiler HDF5 file?", sep = "")),
                  style = "danger", dismiss = FALSE)
    })
  })

  h5_metadata <- reactive({
    if (is.null(h5_measurements())) {
      return(character())
    }
    metadata <- getMetadataFromMeasurements(h5_measurements())
    return(metadata)
  })

  h5_objects <- reactive({
    if (is.null(h5_measurements())) {
      return(character())
    }
    objects <- getObjectsFromMeasurements(h5_measurements())
    return(objects)
  })

  h5_features <- reactive({
    if (is.null(h5_measurements())) {
      return(character())
    }
    features <- getFeaturesFromMeasurements(h5_measurements(), selected_objects_vector())
    return(features)
  })

  layout <- reactive({
    if (is.null(layout_file())) {
      closeAlert(session, "layout_reading_alert")
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

  plate_ids_from_layout <- reactive({
    if(is.null(layout())) {
      return(c())
    }
    as.vector(unique(layout()$plateID))
  })

  output$preview_layout <- renderUI({
    if(is.null(layout())) {return(NULL)}
    dataTableOutput(ns("layout_dt"))
  })

  output$layout_dt <- renderDataTable(getOrElse(layout(),data.frame()), options = list(pageLength = 5,
                                                               lengthMenu = c(5, 10, 15, 20)))
  selected_objects <- reactive({
    list(
    "parent-object" = input$primary_object,
    "child-objects" = input$secondary_object,
    "tertiary-object" = input$tertiary_object
    )
  })

  selected_objects_vector <- reactive({
    return(unlist(selected_objects(), use.names = FALSE))
  })

  extractHDF5_metadata_config <- reactive({
    list(
      "location-id" = c(input$location_id),
      "plate-id" = c(input$plate_id),
      "image-id" = c(input$image_id),
      "time-id" = c(input$time_id),
      "replicate-id" = c(input$replicate_id),
      "exposure-delay" = strftime(input$exposure_delay, "%R"),
      "time-between-frame" = strftime(input$time_between_frame, "%T"),
      "objects" = selected_objects(),
      "features"  = input$features
    )
  })

  extractHDF5Config <- reactive({
    list(
      "input" = extractHDF5_input_config(),
      "metadata" = extractHDF5_metadata_config(),
      "show-results" = input$show_results
    )
  })

  return(extractHDF5Config)
}
