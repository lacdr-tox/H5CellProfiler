#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinyFiles)
library(parallel)
source("../utils/cphdf5.R")
source("utils.R")
source("trackingModule.R")


modules <- c("extract-hdf5", "tracking")
module_labels <- c("extract-hdf5" = "Extract HDF5", "tracking" = "Tracking")
default_modules <- c("extract-hdf5")
max_cores <- detectCores()


selected_h5 <- NULL
selected_metadata <- ""

getHelpTextForModule <- function(module) {
  return(paste("Here you can set the settings for the", module_labels[[module]], "module"))
}

# Define UI for application that draws a histogram
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
                   min=1, max=max_cores, value=max_cores, step=1),

       checkboxGroupInput("modules", label = "Modules to run:",
                          choices = as.list(invertVector(module_labels)),
                          selected = default_modules)
     ),

     mainPanel(
       h3("Module settings"),
       tabsetPanel(
         tabPanel(module_labels[["extract-hdf5"]],
           helpText(getHelpTextForModule("extract-hdf5")),
           uiOutput("hdf5s"),
           uiOutput("metatsv"),
           dataTableOutput("view-meta"),
           checkboxInput("show-results", "Show results in browser")
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

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  roots <- getVolumes()

  shinyDirChoose(input, 'input-directory', session=session, roots = roots)
  shinyDirChoose(input, 'output-directory', session=session, roots = roots)

  inputDirectory <- reactive(parseDirPath(roots, input$`input-directory`))
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

  getH5Files <- function() {
    dir(inputDirectory(), "*.h5")
  }

  h5files_in_inputdir <- reactivePoll(1000, session, getH5Files, getH5Files)

  output$hdf5s <- renderUI({
    tags <- list()

    choices <- h5files_in_inputdir()
    selected <- intersect(choices, selected_h5)

    tags[["h5check"]] <- checkboxGroupInput("h5files", label = "HDF5 files to use:",
                       choices = choices,
                       selected = selected)
    if(length(inputDirectory()) == 0) {
      # No input directory set
      tags[["nodirset"]] <- helpText("Please set input directory in the left panel")
    } else if(length(h5files_in_inputdir()) == 0) {
      # Input directory set, but no HDF5 files
      tags[["noh5indir"]] <- helpText("No HDF5 files in input directory")
    }

    # Convert our list of 'tags' to a proper tagList
    do.call(tagList, tags)
  })

  getPossibleMetadataFiles <- function() {
    dir(inputDirectory(), "*.tsv|*.txt")
  }

  tsvfiles_in_inputdir <- reactivePoll(1000, session, getPossibleMetadataFiles, getPossibleMetadataFiles)

  output$metatsv <- renderUI({
    tags <- list()

    choices <- c("None selected" = "", tsvfiles_in_inputdir())
    selected <- ifelse(selected_metadata %in% choices, selected_metadata, "")
    tags[["tsvcheck"]] <- radioButtons("tsvfile", label = "Metadata TSV file to use:",
                       choices = choices, selected = selected)
    if(length(inputDirectory()) == 0) {
      # No input directory set
      tags[["nodirset"]] <- helpText("Please set input directory in the left panel")
    } else if(length(tsvfiles_in_inputdir()) == 0) {
      # Input directory set, but no possible tsv files
      tags[["notsvindir"]] <- helpText("No tsv/txt files in input directory")
    }

    # Convert our list of 'tags' to a proper tagList
    do.call(tagList, tags)
  })

  metadatafile <- reactive({
    if (length(inputDirectory()) == 0 || !(!is.null(input$tsvfile) && !nchar(input$tsvfile) == 0) ) {
      return(NULL)
    }
    file.path(inputDirectory(), input$tsvfile)
  })

  metadata <- reactive({
    if (is.null(metadatafile())) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.table(metadatafile(), sep = "\t", header = TRUE, comment.char = "")
  })

  output$`view-meta` <- renderDataTable(metadata(),
                                        options = list(
                                          pageLength = 5,
                                          lengthMenu = c(5, 10, 15, 20)))
  observe({
    selected_h5 <<- input$h5files
  })

  observe({
    selected_metadata <<- input$tsvfile
  })
  tracking_config <- callModule(tracking, "tracking1")
  observe(print(tracking_config()))
})


# Run the application
shinyApp(ui = ui, server = server)

