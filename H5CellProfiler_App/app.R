#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinyFiles)
library(DT)
library(parallel)

source("../utils/cphdf5.R")
source("utils.R")
source("trackingModule.R")
source("extractHDF5Module.R")


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
       shinyDirButton("input-directory", "Set input directory...", "Select input directory"),
       verbatimTextOutput('input-directory'),
       shinyDirButton("output-directory", "Set output directory...", "Select output directory"),
       verbatimTextOutput('output-directory'),

       sliderInput("cores", "CPU cores to use:",
                   min=1, max=max_cores, value=max_cores, step=1, post = " cores"),

       checkboxGroupInput("modules", label = "Modules to run:",
                          choices = as.list(invertVector(module_labels)),
                          selected = default_modules)
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

   ),
   hr(),
   downloadButton("downloadYaml", "Download config file...")
   #actionButton("runConfig", "Run pipeline with config")
))

server <- shinyServer(function(input, output, session) {
  roots <- getVolumes()

  shinyDirChoose(input, 'input-directory', session=session, roots = roots)
  shinyDirChoose(input, 'output-directory', session=session, roots = roots)

  inputDirectory <- reactive({
    if(is.null(input$`input-directory`)) {
      #return(NULL)
      return('~/test/test_yaml')
    }
    parseDirPath(roots, input$`input-directory`)
    })
  outputDirectory <- reactive(parseDirPath(roots, input$`output-directory`))

  output$`input-directory` <- renderText({inputDirectory()})
  output$`output-directory` <- renderText({
    if(!is.null(input$`output-directory`)) {
      return(outputDirectory())
    }
    # If output directory is not set, make it the same as the input directory
    if(!is.null(input$`input-directory`)) {
      return(file.path(inputDirectory(), "output"))
    }
  })

  extractHDF5_config  <- callModule(extractHDF5, "extract_hdf5_1", inputDirectory)
  tracking_config <- callModule(tracking, "tracking1")
  #observe(print(tracking_config()))
  observe(print(extractHDF5_config()))
})



# Run the application
shinyApp(ui = ui, server = server)

