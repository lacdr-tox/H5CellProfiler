
# Application breaking bug :(
# https://github.com/rstudio/shiny/issues/1244

library(shiny)
library(shinyBS)
library(shinyTime)
library(DT)
library(parallel)

source("../utils/cphdf5.R")
source("utils.R")
source("trackingModule.R")
source("extractHDF5Module.R")


app_version <- "0.1.0"
modules <- c("extract-hdf5", "tracking")
module_labels <- c("extract-hdf5" = "Extract HDF5", "tracking" = "Tracking")
default_modules <- c("extract-hdf5")
max_cores <- detectCores()

getHelpTextForModule <- function(module) {
  return(paste("Here you can set the settings for the", module_labels[[module]], "module"))
}

ui <- shinyUI(fluidPage(

   # Application title
   titlePanel("H5CellProfiler"),
       helpText("(Hopefully) helps you setting parameters for the H5CellProfiler pipeline"),

   sidebarLayout(
     sidebarPanel(
       h3("General settings"),

       tags$label(`for` = "used_input_directory", "Input directory:"),
       verbatimTextOutput('used_input_directory'),
       textInput("input_directory", "Override input directory:", ""),
       tags$label(`for` = "used_output_directory", "Output directory:"),
       verbatimTextOutput('used_output_directory'),
       textInput("output_directory", "Override output directory:", ""),

       sliderInput("cores", "CPU cores to use:",
                   min=1, max=max_cores, value=max_cores, step=1, post = " cores"),

       checkboxGroupInput("modules", label = "Modules to run:",
                          choices = as.list(invertVector(module_labels)),
                          selected = default_modules),
       downloadButton("download_yaml", "Download config file...")
     ),

     mainPanel(
       h3("Module settings"),
       tabsetPanel(
         tabPanel(module_labels[["extract-hdf5"]],
           helpText(getHelpTextForModule("extract-hdf5")),
           extractHDF5UI("extract_hdf5_1")
         ),
         tabPanel(module_labels[["tracking"]],
           helpText(getHelpTextForModule("tracking")),
           trackingUI("tracking1")
         )
       )
     )
   )
))

getServer <- function(input.dir) {
  shinyServer(function(input, output, session) {

    inputDirTextInput  <- reactive({
      if(input$input_directory == "") {
        return(NULL)
      }
      print(input$input_directory)
      return(input$input_directory)
    })
    outputDirTextInput  <- reactive({
      if(input$output_directory == "") {
        return(NULL)
      }
      return(input$output_directory)
    })

    inputDirectory <- reactive({
      if(is.null(inputDirTextInput())) {
        return(input.dir)
      }
      return(inputDirTextInput())
    })
    outputDirectory <- reactive({
      if(!is.null(outputDirTextInput())) {
        return(outputDirTextInput())
      }
      return(file.path(inputDirectory(), "H5CP_output"))
    })
    
    output$used_input_directory <- renderText(inputDirectory())
    output$used_output_directory <- renderText(outputDirectory())

    extractHDF5_config  <- callModule(extractHDF5, "extract_hdf5_1", inputDirectory)
    tracking_config <- callModule(tracking, "tracking1")

    modules_config <- reactive({
      list(
        "extract-hdf5" = extractHDF5_config(),
        "tracking" = tracking_config()
      )
    })

    io_config <- reactive({
      list(
        "input-directory" = inputDirectory(),
        "output-directory" = outputDirectory()
      )
    })

    config <- reactive({
      list(
        "config-version" = app_version,
        "io" = io_config(),
        "run" = input$modules,
        "cores" = input$cores,
        "modules" = modules_config()
      )
    })

    output$download_yaml <- downloadHandler(
      filename = "config.yaml",
      content = function(file) {
        cat(yaml::as.yaml(config()), file = file)
      },
      contentType = "application/x-yaml"
    )
  })
}

getApp <- function(input.dir = getwd()) {
  server <- getServer(input.dir)
  # launch.browser should be true, otherwise downloadHandler is not working
  shinyApp(ui = ui, server = server, options = list(launch.browser=T))
}
