
# Application breaking bug :(
# https://github.com/rstudio/shiny/issues/1244

library(shiny)
library(shinyBS)
library(shinyFiles)
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
       shinyDirButton("input_directory", "Set input directory...", "Select input directory"),
       verbatimTextOutput('input_directory'),
       shinyDirButton("output_directory", "Set output directory...", "Select output directory"),
       verbatimTextOutput('output_directory'),

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

server <- shinyServer(function(input, output, session) {
  roots <- getVolumes()

  shinyDirChoose(input, 'input_directory', session=session, roots = roots)
  shinyDirChoose(input, 'output_directory', session=session, roots = roots)

  inputDirShinyFiles  <- reactive({
    if(is.null(input$input_directory)) {
      return(NULL)
    }
    return(parseDirPath(roots, input$input_directory))
  })
  outputDirShinyFiles <- reactive({
    if(is.null(input$output_directory)) {
      return(NULL)
    }
    return(parseDirPath(roots, input$output_directory))
  })

  inputDirectory <- reactive({
    if(is.null(inputDirShinyFiles())) {
      #return(NULL)
      return('~/test/test_yaml')
    }
    return(inputDirShinyFiles())
  })
  outputDirectory <- reactive({
    if(!is.null(outputDirShinyFiles())) {
      return(outputDirShinyFiles())
    }
    if(!is.null(inputDirectory())) {
      return(file.path(inputDirectory(), "output"))
    }
    return(NULL)
  })

  output$input_directory <- renderText(inputDirectory())
  output$output_directory <- renderText(outputDirectory())

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



# Run the application
shinyApp(ui = ui, server = server)

