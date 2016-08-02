# Tracking Shiny Module
source("utils.R")

### GLOBAL STUFF ###

unique_parent_solutions <- c(
  "min_distance" = "Based on minimal distance",
  "disconnect_all" = "Disconnect all children from shared parent")
preferred_unique_parent_solution <- "disconnect_all"
unique_parent_solutions[[preferred_unique_parent_solution]] <-
  paste(unique_parent_solutions[[preferred_unique_parent_solution]], "(preferred)")

extra_output_files_list <- c(
  "unique-parents-no-rec" = "Tracking data after assigning unique parents (before reconnecting)",
  "before-combine-tracks" = "Feature data before reconnecting",
  "after-first-connect" = "Feature data after reconnecting over 1 frame",
  "after-second-connect" = "Feature data after reconnecting over 2 frames",
  "after-third-connect" = "Feature data after reconnecting over 3 frames")
default_output_files <- NULL

reconnect_over_frames <- c(
  "0 (don't reconnect over frames)" = 0,
  "1 (connect consecutive frames)" = 1,
  "2 (skip one frame)" = 2,
  "3 (skip two frames)" = 3
)
default_reconnect_over_frames <- 0

full_width <- validateCssUnit("100%")

stored_reconnect_distances <- list(0,0,0)

### TRACKING UI ###

trackingUI <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Assign unique parents"),
    helpText("This setting controls how the pipeline should resolve shared parents.
             The older versions assigned a unique parent based on minimal distance.
             The newer (preferred) solution completely disconnects all children that share a
             parent."),
    radioButtons(ns("parent_resolve_strategy"), NULL, invertVector(unique_parent_solutions),
                 selected = preferred_unique_parent_solution, width = full_width),
    h4("Reconnect tracks"),
    helpText(HTML("These settings control if and how tracks should be reconnected.")),
    bsAlert(ns("reconnect_tracks_alert")),
    selectInput(ns("reconnect_over_frames"), "Maximum number of frames to reconnect over:",
                choices = reconnect_over_frames,
                selected = default_reconnect_over_frames,
                width = full_width),
    bsAlert(ns("reconnect_slider_alert")),
    uiOutput(ns("reconnect_distance")),
    h4("Discard short tracks"),
    helpText(HTML("This setting sets the minimum desired track length, tracks that have a shorter
                  length (after reconnecting) will be thrown away.")),
    bsAlert(ns("discard_short_tracks_alert")),
    numericInput(ns("min_tracked_frames"), label = "Minimum track length:", value = 1, min = 1,
                 step = 1),
    h4("Output files"),
    helpText("Here you can specify which extra output files you want.
             These are mostly used for testing so it's totally safe to leave them unchecked"),
    checkboxGroupInput(ns("write"), label = NULL,
                       choices = as.list(invertVector(extra_output_files_list)),
                       selected = default_output_files,
                       width = full_width)

  )
}

### TRACKING SERVER ###

tracking <- function(input, output, session){
  ns <- session$ns

  createAlert(session, ns("reconnect_tracks_alert"), "reconnect_tracks_alert", content =
                HTML("<strong>Warning!</strong> Agressive reconnecting might mask tracking problems
                     and introduce fake effects, use with caution."), style = "warning", dismiss =
                FALSE)
  createAlert(session, ns("discard_short_tracks_alert"), "discard_short_tracks_alert", content =
                HTML("<strong>Warning</strong> Discarding track data is potentially dangerous. You
                     can always filter out short tracks in a later analysis."), style = "warning",
              dismiss = FALSE)

  output$reconnect_distance <- renderUI({
    post <- " pixels"
    pre_label <- "Maximal distance for reconnecting over"
    if(input$reconnect_over_frames == 0) {
      closeAlert(session, "reconnect_slider_alert")
      return(NULL)
    }
    createAlert(session, ns("reconnect_slider_alert"), "reconnect_slider_alert",
                content = HTML("<strong>Warning</strong> Because of a bug in Shiny these sliders
                               can reset when you change the directories in the left panel."),
                style = "warning", dismiss = FALSE)

    tags <- list()
    if(input$reconnect_over_frames > 0) {
      tags[["reconnect_1"]] <- sliderInput(ns("reconnect_1"), paste(pre_label, "1 frame"), 0, 100,
                                           value = stored_reconnect_distances[[1]], step = 1,
                                           post = post, width = full_width)
    }
    if(input$reconnect_over_frames > 1) {
      tags[["reconnect_2"]] <- sliderInput(ns("reconnect_2"), paste(pre_label, "2 frames"),
                                           stored_reconnect_distances[[1]], 100,
                                           value = stored_reconnect_distances[[2]], step = 1,
                                           post = post, width = full_width)
    }
    if(input$reconnect_over_frames > 2) {
      tags[["reconnect_3"]] <- sliderInput(ns("reconnect_3"), paste(pre_label, "3 frames"),
                                           stored_reconnect_distances[[2]], 100,
                                           value = stored_reconnect_distances[[3]], step = 1,
                                           post = post, width = full_width)
    }
    do.call(tagList, tags)
  })

  reconnect_distances <- reactive({
    # Get values from the sliders, but if there is no slider (value NULL) give a 0 instead
    distances <- list(
      getOrElse(input$reconnect_1, 0),
      getOrElse(input$reconnect_2, 0),
      getOrElse(input$reconnect_3, 0))
    # If the slider disappears there is no disappearence event so we set the value to 0 manually
    distances <- lapply(seq_along(distances), function(index, distances){
      ifelse(index <= input$reconnect_over_frames, distances[[index]], 0)}, distances = distances)
    distances <- as.integer(distances)
  })

  observe({
    # Weird construction to enable variable saving on redraw of renderUI
    stored_reconnect_distances <<- reconnect_distances()
  })

  # If slider 1 gets updated, use it as a lower bound for 2, we use isolate because we don't want a
  # reactive dependency.
  observeEvent(input$reconnect_1, {
    isolate({
      if(is.null(input$reconnect_2)) {return()}
      updateSliderInput(session, "reconnect_2", min = input$reconnect_1)
    })
  })

  # If slider 2 gets updated, use it as a lower bound for 3
  observeEvent(input$reconnect_2, {
    isolate({
      if(is.null(input$reconnect_3)) {return()}
      updateSliderInput(session, "reconnect_3", min = input$reconnect_2)
    })
  })

  tracking_write_config <- reactive({
    # Try to find the options in the checkboxes
    are_names_selected <- names(extra_output_files_list) %in% input$write
    # Add the correct names to the logical vector
    names(are_names_selected) <- names(extra_output_files_list)
    # Convert logical vector to list
    as.list(are_names_selected)
  })

  # build the complete config for the tracking module
  tracking_config <- reactive({
    list(
    "parent-resolve-strategy" = input$parent_resolve_strategy,
    "reconnect-tracks" = (input$reconnect_over_frames > 0),
    "reconnect-over-frames" = as.integer(input$reconnect_over_frames),
    "max-pixel-reconnect" = unlist(reconnect_distances()),
    "min-tracked-frames" = input$min_tracked_frames,
    "write" = tracking_write_config())
  })

  return(tracking_config)
}
