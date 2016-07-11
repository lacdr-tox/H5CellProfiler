
# server.R
source("../theme_sharp.R")
values <- reactiveValues()

## TODO: multiple features plotten
## TODO: data selection

shinyServer(
  function(input, output, session) {
        updateData <- function() {
        values[["myDFo"]] <- get("myDFo", env = .GlobalEnv)
}
    updateData()
    updateData2 <- function() {
      values[["outputList"]]<-NULL
      vars <- load(paste(input$path, "outputList.Rdata", sep ="/"))
      values[["outputList"]] <- get(vars, .GlobalEnv)
        if(length(unlist(lapply( lapply(outputList, names),str_match_all, "myDT") )) > 1) {
            x <- rbindlist(lapply(values[["outputList"]], "[[", "myDT"))
        } else{
           x <- outputList$myDT
      }
    return(x)           
  }
    updateData3 <- function(){
                            values[["sumData"]] <- get("sumData", env = .GlobalEnv)
  }
        updateData3()
                # update certain ui variables:
                observe({
                    if( input$goButton5 == 0)
                        return()
                        isolate({
                        # re-do colnames of updated myDFo
                        updateSelectInput(session, "idsChoice", 
                                      choices = c(colnames(values[["myDFo"]]), colnames(values[["sumData"]])))
                    })
                })
                                observe({
                                  if( input$goButton5 == 0)
                                    return()
                                  isolate({
                                  updateSelectInput(session, "nomiCol", 
                                                    choices = colnames(values[["myDFo"]]))
                                  })
                                })
                      observe({
                        if( input$goButton5 == 0)
                          return()
                        isolate({
                        updateSelectInput(session, "denomiCol", 
                                          choices = colnames(values[["myDFo"]]))
                        })
                      })
            observe({
              if( input$goButton5 == 0)
                return()
              isolate({
              updateSelectInput(session, "rmCol", 
                                choices = colnames(values[["myDFo"]]))
              })
            })
  observe({
    if( input$goButton5 == 0)
      return()
    isolate({
    updateSelectInput(session, "filtCol", 
                      choices = colnames(values[["myDFo"]]))
    })
  })
    datasetInput <- reactive({
      switch(input$dataset,
             "Single Cell" = values[["myDFo"]],
             "Summary Data" = values[["sumData"]])
        })
        #division function
        divFun <- function(x){
            denomiCol<-input$denomiCol
            nomiCol <- input$nomiCol
            kAddDenom<-input$kAddDenom
            meanOne <- x[,mean(as.numeric(get(denomiCol)), na.rm=TRUE) ]
            kAddD <- kAddDenom * meanOne
            x[, dummy:= as.numeric(get(denomiCol))+kAddD]
            x[, dummy2:= as.numeric(get(nomiCol)) / as.numeric(dummy)]
            x[, dummy:=NULL]
            x[is.infinite(dummy2), dummy2:=NA] 
            meanDivOne <- x[,mean(dummy2, na.rm=TRUE) ]        
            setnames(x, 'dummy2', paste(nomiCol, denomiCol, sep =".DIV."))
            x  
         }
          divFunReactive <- reactive({
                            divFun(values[["myDFo"]])
        })
              observe({
                if (input$goButton3 == 0)
                   return()
                     isolate({   
                         assign(x= "myDFo", value = divFunReactive(), envir = .GlobalEnv )
                  updateData()
               })
            })
    # delete column function
    rmColFun <- function(x){
               indr <- which(colnames(x) == input$rmCol)
                x[, eval(indr):=NULL]
   }
        rmColFunReactive <- reactive({
                            rmColFun(values[["myDFo"]])
     })
            observe({
              if (input$goButton2 == 0)
                return()
              isolate({
                rmColFunReactive()
                updateData()
              })
          })
# display 100 random rows of single cell data table
observe({
     if (input$goButton4 == 0)
         return()
       isolate({
           updateData()
          set.seed(12345)
          output$myTable <- renderTable({
           values[["myDFo"]][sample(nrow(values[["myDFo"]]), 100, replace=TRUE), ]
       })
     })
  })
# filter function
filterFun <- function(x) {
              if( input$opera == '>'){ 
                      x <- x[ get(input$filtCol) > as.numeric(input$filtVal) ]
                    } else if (input$opera == '<'){
                          x <- x[ get(input$filtCol) < as.numeric(input$filtVal)]
                } else {
                  warning("Choose > or <")
              }
    return(x)
}
      filterFunReactive <- reactive({
                          filterFun(values[["myDFo"]])
  })
                      observe({
                        if (input$goButton6 == 0) 
                return()
        isolate({
             assign(x="myDFo",value=filterFunReactive(), envir= .GlobalEnv)  
             updateData()
  })    
})

##TODO: normalizatie opties. veranderen per gekozen factor
## TODO: only min max is working as intended atm
  plateNormFun <- function(y){
  # calculate mean per plate, use these values for min max values instead of single cell data which is too sensitive to single outlier cells
          x <-  y[, lapply(.SD, function(x) { mean(as.numeric(x), na.rm = TRUE)}
          ), 
          by =  eval(input$by.what),
          .SDcols = input$idsChoice[1]] # create summary
              if(input$normalization == "MinMax"){
                      paraD<-x[, list(MIN=min(as.numeric(get(input$idsChoice[1])), na.rm = TRUE),
                                      MAX=max(as.numeric(get(input$idsChoice[1])), na.rm = TRUE)
                                      ), by =  plateID]
                                        
            setkey(y,"plateID")
            setkey(paraD, "plateID")
            x<-y[paraD]
            x[ , dummy:= (get(input$idsChoice[1]) - MIN) / (MAX-MIN) ]
            setnames(x, "dummy", paste("min_maxNorm", as.character(input$idsChoice[1]), sep ="_"))
            x[, MIN:=NULL]
            x[, MAX:=NULL]
     
                } else if( input$normalization == "Z-Score"){
                              warning("z-score in on todo list")
                                      if(input$negativeControl != ""){
                                        negCstring <- as.character(input$negativeControl)
                                      if(!"control" %in% colnames(x)){
                                          stop("Include control in summary data")
                                    }
                                    setkey(x, "control")
                                    controlD<- x[control==negCstring]
                                    paraD <- controlD[, list(SD=sd(as.numeric(get(input$idsChoice[1])), na.rm = TRUE),
                                                             MEAN=mean(as.numeric(get(input$idsChoice[1])), na.rm = TRUE)),
                                                        by =  plateID] 
                                    setkey(x,"plateID")
                                    setkey(paraD, "plateID")
                                    x <- x[paraD]
                                    x[ , dummy:= (get(input$idsChoice[1]) - MEAN) / (SD) ]
                                    setnames(x, "dummy", paste("negC_Z-score", input$idsChoice[1]))
                                    x[, MEAN:=NULL]
                                    x[, SD:=NULL]
                                
                                    }else  { #plate based calculations
                                          paraD<-x[, list(SD=sd(as.numeric(get(input$idsChoice[1])), na.rm = TRUE),
                                                          MEAN=mean(as.numeric(get(input$idsChoice[1])), na.rm = TRUE)),
                                                   by =  plateID]
                                          setkey(x,"plateID")
                                          setkey(paraD, "plateID")
                                          x <- x[paraD]
                                          x[ , dummy:= (get(input$idsChoice[1]) - MEAN) / (SD) ]
                                          setnames(x, "dummy", paste("plate_Z-score", input$idsChoice[1]))
                                          x[,MEAN:=NULL]
                                          x[, SD:=NULL]
                                          
                                           }
    
                    }else if( input$normalization == "Median"){
                      warning("median is on todo list")
                      paraD<-x[, list(MEDIAN=median(as.numeric(get(input$idsChoice[1])), na.rm = TRUE)),
                               by =  plateID]
                      
                      setkey(x,"plateID")
                      setkey(paraD, "plateID")
                      x <- x[paraD]
                      x[ , dummy:= (get(input$idsChoice[1]) / MEDIAN) ]
                      setnames(x, "dummy", paste("medianScaled", input$idsChoice[1]))
                      x[,MEDIAN:=NULL]
              }else{
                 warning("normalization error")
      }
  xy <- list(paraD=paraD, normData=x)
  xy
}
plateNormFunReactive <- reactive({plateNormFun(
                            values[["myDFo"]] # normalizations based on summarized data by plateID and wellID of myDFo
)})
 observe({
       if (input$goButton9 == 0) 
           return()
            isolate({
                assign("myDFo", plateNormFunReactive()$normData, envir = .GlobalEnv)
                updateData()
                  output$tempData <- renderTable({
                            plateNormFunReactive()$paraD
                    })
             })
   })
# count function
countFun <- function(x) {
                if( input$opera == '>'){ 
                    ope <-"larger"
                    x[, dummyC:=as.integer(get(input$filtCol) > as.numeric(input$filtVal)) ]
                        } else if (input$opera == '<'){
                              x[, dummyC:=as.integer(get(input$filtCol) < as.numeric(input$filtVal)) ]
                              ope <-"smaller"
                  } else {
                    warning("Choose > or <")
                  }
            if(input$naIsZero){
                  x[ is.na(get(input$filtCol)) , dummyC:= 0   ]
            }
      setnames(x, "dummyC", paste("count",  eval(input$filtCol), ope ,input$filtVal, input$ncolName, sep ="_") )
  return(x)
}
      countFunReactive <- reactive({
              countFun(values[["myDFo"]])
      })
            observe({
              if (input$goButton8 == 0) 
                return()
                  isolate({
                      assign("myDFo", countFunReactive(), envir =.GlobalEnv)  
                      updateData()
                  })
              })
#select function
selFun <- function(x) {
         x<- x[ get(input$filtCol) %in% eval(input$filtVal)]
         return(x)
}
        selFunReactive <- reactive({
          selFun(values[["myDFo"]])
        })
              observe({
                if (input$goButton11 == 0) 
                  return()
                  isolate({
                      assign("myDFo", selFunReactive(), envir =.GlobalEnv)  
                      updateData()
                })
            })
##TODO make backup function for backup and retrieval in R memory
# reload data
observe({
    if(input$goButton7 == 0)
      return()
          isolate({
              if(nchar(input$path)>1){
                  values[["myDFo"]]<-NULL 
                  values[["myDFo"]]<-updateData2() # reload data from Rdata file
             assign("myDFo", value = values[["myDFo"]], envir = .GlobalEnv) 
                  } else{
                        warning("define path to RData file, use forward slashes")
                  }
          })
})
# summarization function
makeSum <- function(x){
                     x<- x[, lapply(.SD, (switch(input$summFunChoice, 
                              "mean" = function(x) { mean(as.numeric(x), na.rm = TRUE)},
                              "sd" = function(x) { sd(as.numeric(x), na.rm = TRUE)},
                              "sum" = function(x) { sum(as.numeric(x), na.rm =TRUE)},
                              "median" = function(x) { median(as.numeric(x), na.rm=TRUE) },
                              "q10" = function(x) { quantile(as.numeric(x), 0.1, na.rm = TRUE)},
                              "q90" = function(x) { quantile(as.numeric(x), 0.9, na.rm = TRUE)},
                              "min" = function(x) { min(as.numeric(x), na.rm = TRUE)},
                              "max" = function(x) { max(as.numeric(x), na.rm = TRUE)}
                                        )
                                  )
                            ), 
                            by =  eval(input$by.what),
                            .SDcols = input$idsChoice]
               if(length(input$idsChoice) > 1){
                    x <- melt(x, measure.vars = input$idsChoice)
                } 
      return(x)
}
      makeSumReactive <- reactive({ 
        makeSum(values[["myDFo"]])
      })
              observe({ 
                  if(input$goButton == 0)
                     return()
                    isolate({
                            assign("sumData", value = makeSumReactive(), envir =.GlobalEnv)
                            updateData3()
                            output$SummaryData <- renderTable({
                                    # sumData as global variable
                                    n.row <- nrow(values[["sumData"]])
                                    if(n.row < 1001){ 
                                    values[["sumData"]]
                                          } else {
                                            values[["sumData"]][sample(1:n.row, 1000, replace = TRUE), ]
                                            }
                            },digits = 3)
                    })
              })
# function that creates levels within factors
makeDoseFacFun <- function(x){
                  piDataL <- as.data.frame(x)
                  piDataL <- piDataL[ order( piDataL[ , "treatment"], piDataL[ , "dose_uM"]),   ]
                  counts.d <- by(data = piDataL, INDICES = piDataL[, "treatment"], function(x) d.levels = unique(x[, "dose_uM"]))
                        if(sum(lapply(counts.d, length ) >1  ) > 0 ) {  # if any compound has more than 1 dose level:
                              counts.d.l <- sapply(counts.d, as.list)
                              counts.d.l <- melt(counts.d.l,  length)
                              old.nrow <- nrow(piDataL)
                              piDataL <- merge(piDataL, counts.d.l, by.x = c( "treatment", "dose_uM"), by.y = c( "L1", "value"), sort = FALSE)
                                    if(nrow(piDataL) != old.nrow){
                                       stop("setting dose levels for density plots failed")
                                    }
                              piDataL$L2 <- factor(piDataL$L2)
                              colnames(piDataL)[ colnames(piDataL) == "L2"] <- "doseLevel"
                         }
                  return(as.data.table(piDataL))
}
        makeDoseFacFunReactive <- reactive({
          makeDoseFacFun(values[["sumData"]])
        })
              observe({ 
                  if(input$goButton12 == 0)
                    return()
                        isolate({
                          assign("sumData", value = makeDoseFacFunReactive(), envir =.GlobalEnv)
                          updateData3()
                        })
              })
# after table modifications variable display in GUI has to be updated:
observe({
  if( input$goButton5 == 0)
      return()
        isolate({
        # re-do colnames of updated myDFo
            updateSelectInput(session, "onXaxis", 
                              choices = colnames(values[["sumData"]]))
        })
})
            observe({
              if( input$goButton5 == 0)
                  return()
                    isolate({
                          updateSelectInput(session, "Color", 
                                            choices = colnames(values[["sumData"]]))
                    })
            })
                        observe({
                          if( input$goButton5 == 0)
                              return()
                                isolate({
                                      updateSelectInput(session, "connectDots", 
                                                        choices = c(" ",colnames(values[["sumData"]])))
                                })
                        })
                        observe({
                          if( input$goButton5 == 0)
                              return()
                                isolate({
                                      updateSelectInput(session, "Shape", 
                                                        choices = c("",colnames(values[["sumData"]])))
                                })
                        })
            observe({
              if( input$goButton5 == 0)
                return()
              isolate({
                updateSelectInput(session, "yVar", 
                                  choices = colnames(values[["sumData"]]))
              })
            })
observe({
  if( input$goButton5 == 0)
    return()
  isolate({
    updateSelectInput(session, "xVar", 
                      choices = colnames(values[["sumData"]]))
  })
})
# plot facetting function
plotAll <- function(x) {
              chosenVar <<- input$idsChoice[[1]]
              chosenVars<<- input$idsChoice
              Color<-input$Color
              if(Color == " "){
                Color <-NULL
              }
              Fill <- input$fill
              if(Fill == " "){
                Fill <- NULL
              }
              Shape <- input$Shape
              if(Shape == " "){
                Shape<-NULL
              }
              pointSize <- input$pointSize
              if(as.numeric(pointSize)%in% 1:10){
                pointSize <- as.numeric(pointSize)
              }       
              connectDots <- input$connectDots
              if(connectDots == " "){
                connectDots<-NULL
              }
              onXaxis <- input$onXaxis
              xScales<-input$xScales
              yScales<-input$yScales
              Scales <- unique(xScales, yScales)
              #Color Shape connectDots
              if(any(Scales %in% c("free_x","free_y"))){
                Scales <- Scales[!Scales %in% "fixed"]  
              }
              if(input$yMin!= " " ){
                yMin <- as.numeric(input$yMin)
                } else {
                  yMin <- NULL
                }
              if( input$yMax != " "){
                yMax <- as.numeric(input$yMax)
                } else {
                  yMax <- NULL
                }
              if(input$facets[1] == " ") {
                facetVar <-NULL
                } else  if(length(input$facets)==1){
                  facetVar <- as.formula(paste("~", input$facets, sep =""))
                  } else if(length(input$facets) == 2){
                    facetVar <- as.formula(paste(input$facets[1], "~", input$facets[2], sep =""))
                
                    } else if(length(input$facets) == 3){
                      facetVar <- as.formula(paste(input$facets[1], "~", input$facets[2], "~", input$facets[3], sep =""))
                      }
                      else {
                          warning("max two facets")
                          }
              if(length(chosenVars) >1 ) {
                    chosenVar <- "value"
              }
              if(input$plotType == "line plot"){
                sumData[, eval(Color):= as.factor(get(Color))]
                p<- ggplot( data =x,  aes_string( x = onXaxis , y = chosenVar,
                                                colour = Color , shape = Shape)
                            ) +
                                                    geom_point( aes_string(size = pointSize, na.rm = TRUE )) + 
                                                    geom_line( aes_string( group = connectDots, colour = Color)) +
                                                    coord_cartesian( ylim=c(yMin,yMax)  )
                  p <- p + facet_wrap( facetVar , scales = Scales  ) 
                        p <- p + theme_sharp() +
                        theme( axis.text.x = element_text(angle = 90, hjust = 1, size = as.numeric(input$textSize) ) ) + 
                        theme( strip.text.x = element_text( size = as.numeric(input$textSize) )) +
                        ggtitle( "demo version" ) + 
                        theme(plot.title = element_text(lineheight=.8, size = 0.6*as.numeric(input$textSize)))   + theme(legend.position = "bottom") +
                        theme(legend.text=element_text(size= 0.8*as.numeric(input$textSize)))
                        p
                } else { # barplot
                              collapseVar <<- input$collapse
                              barD <- makeSumReactive()
                              barD_L <- melt(barD, measure.vars = eval(chosenVars))
                              aucCols<-input$by.what[!input$by.what %in%  c(collapseVar)]
                              aucCols <- c(aucCols, 'variable')
                              barD <-ddply( barD_L , aucCols, summarize,
                                           collapse = mean(value, na.rm= TRUE))
                              barD<- as.data.frame(barD)    
                              barD$variable <- factor(barD$variable, chosenVars, order = TRUE)
                              toOrder<- barD[ barD$variable==chosenVar,]
                              toOrder <- ddply(toOrder, onXaxis, summarise, m.mean = mean(collapse, na.rm=TRUE))
                              toOrder <- toOrder[ , onXaxis][order(toOrder[, 'm.mean'])]
                              barD[, onXaxis] <- factor(barD[, onXaxis], levels = toOrder, order = TRUE)
                              ##TODO 1) barplot plotten  2) z-score (zorg dat barplot deze kan plotten. 3) tracking functie runnen (values[["myDFO]] ermee updaten)
                              collapseV<- "collapse"
                              
                             p <- ggplot(data = barD, aes_string(x = onXaxis , y = collapseV, colour = Color, fill = Fill))  + 
                                         geom_bar(stat = "identity", position = "dodge") 
                             p <- p +  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.4, size = 12 + round(400/ nrow(barD), 0), 
                                                                         colour = "grey50") ) + theme( strip.text.x = element_text( )) +
                               ggtitle( paste(  "MV_AUC", chosenVars[1]) ) + 
                               theme(plot.title = element_text(lineheight=.8, size = 14 ))
                                    if(length(input$facets == 1)){ 
                                           p <- p + facet_wrap( facetVar ,ncol = 1, scales = "free_y" )
                                        } else{
                                              p <- p + facet_wrap( facetVar , scales = "free_y" )
                                              }
                                pp=list()
                                pp$p <- p
                                pp$barD <- barD
                                pp$sumData <- sumData
                      return(pp)
               }
}
              observe({
                  if(input$button10 == 0)
                    return()
                    output$barD <- renderTable({
                                            plotAll(values[["sumData"]])$barD
                    })
              })
#download data
    output$downloadData <- downloadHandler(
                          filename = function() { paste(input$dataset, '.txt', sep='') },
                            content = function(file) {
                                        write.table( datasetInput(), file, sep = "\t", row.names = FALSE)
                            }
                            )
  # main pdf creator
     output$downloadPlot <- downloadHandler(
                   filename = function() { paste(input$dataset, '.pdf', sep='') },
                      content = function(file) {
                                            pdf(file, width = as.numeric(input$resX), height = as.numeric(input$resY))
                                            print(plotAll(values[["sumData"]]))
                      dev.off()
                      })
# select subset of data based on treatment
datSel <- reactive({
            dat <- values[["sumData"]]
            setkeyv(dat, input$by.what[1])
            datSel <- dat[ input$currTreat ]
    datSel
})
#interactive plotting
vis <- reactive({
      xVar <- prop("x", as.symbol(input$xVar))
      yVar <- prop("y", as.symbol(input$yVar))
      datSel %>% ggvis(x = xVar, y = yVar) %>%
      layer_points(size = 300, fill = ~get(input$mapCol)) %>%
    #add_axis("x", title = xvar_name) %>%
    #add_axis("y", title = yvar_name) %>%
    #add_legend("fill", title = "variable", values = " blue") %>%
      set_options(width = 500, height = 500)
})
    vis %>% bind_shiny("gv")
  }
)




