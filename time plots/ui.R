


all.treats <- (unique(myDFo$treatment))
ids <- colnames(myDFo)
facLevels <- c("treatment","plateID",  "dose_uM", "timeID", 
               "controlID", "replID", "locationID", "control", 
               "cell_line", "imageID", "plateWellID", "timeAfterExposure", kColNames$parentObjectNumberCN)


shinyUI(fluidPage( 
  titlePanel("CellProfiler h5 GUI"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
helpText("Create summary statistics and make 
         interactive plots of CP generated hdf5 data."),

      selectInput("idsChoice", 
                  label = "Choose variables for analysis",
                  choices = c(colnames(myDFo), colnames(sumData)),
                  selected = myFeatures[1],
                  width = "auto",
                  multiple = TRUE
                  ),
      
      selectInput("summFunChoice", 
                  label = "Summary Function:",
                  choices = c("mean", "sd", "sum", "median", "q10", "q90","min","max"),
                  
                  ),
      
      selectInput("by.what", 
                  label = "Summary by what:",
                  choices = facLevels,
                  multiple = TRUE,
                  selected = c("treatment", "timeID", "dose_uM", "plateID")
                  ),
actionButton("goButton", "Calculate summary"),



selectInput("normalization",
            label = "plate normalization method, by display variable:",
            choices = c("MinMax", "Median", "Z-Score")
            ),
textInput("negativeControl",
          label= "negative control string for z-score:",
          value = ""),
actionButton("goButton9", "normalize plates"),
      selectInput("nomiCol",
                  label = "nominator column:",
                  choices = colnames(myDFo),
                  width = "auto"
                  ),
      selectInput("denomiCol",
            label = "denominator column:",
            choices = colnames(myDFo),
            width = "auto"
            
                  ),
      sliderInput("kAddDenom",
            label = "denominator addition by mean multiplier:",
            value = 0,
            min = 0,
            max = 0.05,
            step = 0.001
                  ),
actionButton("goButton3", "Calculate division"),
      
selectInput("rmCol",
            label = "remove column:",
            choices = colnames(myDFo),
            width = "auto"), 

actionButton("goButton2", "remove column!"),

selectInput("filtCol",
            label = "column to filter, count or select:",
            choices = colnames(myDFo),
            width = "auto"), 
textInput("opera",
            label = "> or < (keep/count):"), 
textInput("filtVal",
          label = "filter value"),
actionButton("goButton6", "filter column"),
actionButton("goButton8", "count data"),
actionButton("goButton11", "select data"),
checkboxInput("naIsZero", "count NA is zero", value = TRUE),
textInput("ncolName",
          label = "new column name",
          value = ""
          ),


textInput("path",
          label = "path to Rdata file"),      

actionButton("goButton4", "table preview"),
actionButton("goButton5", "update variables"),
actionButton("goButton7", "reload Data"),
actionButton("button10", "show barD"),

#actionButton("goButton9", "NA is zero convert"),


      selectInput("dataset", "Choose a dataset:", 
                  choices = c("Single Cell", "Summary Data"),
                  selected = "Summary Data"),
      
          downloadButton('downloadData', 'Download')
          

    ),

    mainPanel(
      #SummaryData
      
      tabsetPanel(
        tabPanel("Calculate Summary", tableOutput("SummaryData"),
                 "Normalization (use by what)", tableOutput("tempData")), 
        
                # this panel is for very basic choosing a treatment and plotting & serves as template for further functionality
        tabPanel("plot", ggvisOutput("gv"),
                 selectInput("currTreat", 
                             label = "Choose treatment to plot:",
                             choices = all.treats),
                 
                 selectInput("xVar", 
                             label = "Choose x-axis variable:",
                             choices = colnames(sumData),
                             selected = "timeID"
                             ),
                 
                 selectInput("yVar", 
                             label = "Choose y-axis variable from summaryData:",
                             choices = colnames(sumData)
                             ),
                 selectInput("mapCol", 
                             label = "color mapping:",
                             choices = colnames(sumData))
                 ),
        #This panel is for plotting all treatments in a single pdf. 
        
        tabPanel("plotAll", 
                 checkboxGroupInput("plotType", label = "Choose plot type, for multiple variables make sure to map \"variable\\", 
                                    choices = c("line plot", "bar plot"),
                                    selected ="line plot"),
                 selectInput("collapse", label = "AUC collapse (barplot only):",
                             choices = colnames(sumData)),
                 
                 selectInput("onXaxis", label = "display on x axis:",
                             choices = colnames(sumData)),
                 selectInput("facets", label = "choose facetting variable(s)",
                             choices = c(" ", "variable", colnames(sumData)),
                             multiple = TRUE),
                 selectInput("Color", label ="color mapping",
                             choices = c( " " ,colnames(sumData),"variable"),
                             selected = "plateID"
                             ),
                 selectInput("fill", label ="fill mapping",
                             choices = c( " " ,colnames(sumData),"variable")
                 ),
                 selectInput("Shape", label ="shape mapping(only line-plot)",
                             choices = c( " " , colnames(sumData), "variable")),
                 selectInput("pointSize", label ="dot size",
                             choices = c(1:10, colnames(sumData),"variable")),
                 selectInput("connectDots", label ="connect dots group",
                             choices = c(" " , colnames(sumData),"variable")),
                 selectInput("xScales", label ="X-scale",
                             choices = c("free_x", "fixed"),
                             selected = "fixed"),
                 selectInput("yScales", label ="y-scale",
                             choices = c("free_y", "fixed"),
                             selected ="fixed"),
                 textInput("yMin", label = "y min", value = " "),
                 textInput("yMax", label = "y max", value = " "),
                 textInput("resX","choose pdf width realistic: between 4 and 50", value = 24),
                 textInput("resY", "choose pdf height", value = 20),
                 textInput("textSize", "choose text size", value = 15),
                 actionButton("goButton12", "make dose levels"),
                
                 
                 downloadButton('downloadPlot', 'downloadPlot')
                 ),
        tabPanel("myTable preview", tableOutput("myTable"),tableOutput("barD"))
                  ) ##TODO more panels will be included: AUC/bar plots. Binning plots. Tracking plots - add some usefull plots from older code.
      )
  )

))

