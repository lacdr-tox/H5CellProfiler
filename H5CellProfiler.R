options(stringsAsFactors = FALSE)

pipeline.dir <- getwd()

source("mainFunction.R", chdir = TRUE)
source("fixTrackingFun.R")
source("theme_sharp.R")
source("countCellFun.R")

library(rhdf5)
library(stringr)
library(plyr)
library(data.table)
library(doParallel)
library(ggplot2)
library(reshape2)
library(grid)
library(shiny)
library(ggvis)
library(yaml)

prependInputDirectoryToPath <- function (config) {
  config$modules$`extract-hdf5`$input$hdf5 <-
    file.path(config$io$`input-directory`,
              config$modules$`extract-hdf5`$input$hdf5)
  config$modules$`extract-hdf5`$input$layout <-
    file.path(config$io$`input-directory`,
              config$modules$`extract-hdf5`$input$layout)
  return(config)
}

generateH5CellProfilerVersionReport <- function(pipeline_dir, output_dir) {
  rmarkdown::render(file.path(pipeline_dir, "H5CellProfiler_version_info.Rmd"),
                    output_dir = output_dir,
                    params = list(repo = pipeline_dir))
}

runPipeline <- function(config.file) {
  print(config.file)
  old.wd <- getwd()
  config <- yaml.load_file(config.file)
  config <- prependInputDirectoryToPath(config)
  out.dir <- config$io$`output-directory`
  if(!dir.exists(out.dir)) {dir.create(out.dir)}
  file.copy(config.file, out.dir)
  generateH5CellProfilerVersionReport(pipeline.dir, out.dir)
  setwd(out.dir)
  invisible(lapply(config$run, function (module) {
    print(paste("main - running module", module))
    module.function <- getFunctionForModule(module)
    module.function(config$modules[[module]], config$cores)
  }))
  setwd(old.wd)
  print("main - done")
}

getFunctionForModule <- function(module) {
  switch(module,
         "extract-hdf5"={extractHDF5},
         "tracking"={trackingFun},
         stop(paste("Unknown module", module))
  )
}

extractHDF5 <- function(config, cores) {

  # Each hdf5 file contains 1 or multiple plate-based data (i.e. do not divide data from 1 plate in multiple h5 files unless different time points)
  hdf5FileNameL <- config$input$hdf5 # mainFunction will loop through this vector, at the end rbinding the individual outputs.

  # each hdf5 gets it's own metadata info. Either a path WITH the "Image/" character or a manualy defined string WITHOUT the "/" character
  # If each h5 file has identical metadata 1 entry is sufficient. Else provide entry for each h5 file

  # these metadata variables should be defined in metadata layout file if they vary within the h5 file, if not needed in the metadata file (so metadata is provided in these variables) - then put NA's in the metadata file
  locationID <- config$metadata$`location-id`  # well/ location metadata
  plateID <- config$metadata$`plate-id` # PlateID must always be provided (either manual or h5 paths)- here AND in layout file: becuase plateID is used to couple metadata plate layout file.
  imageID <- config$metadata$`image-id` # Image/Metadata_  ... image number (obtained from image file name )
  timeID <- config$metadata$`time-id` # timeID, either hdf5 path, or vector of numbers according to hdf5 files (each hdf5 is then a time point) So capture time point in h5 file if needed. Or defined in metadata layout file
  replID <- config$metadata$`replicate-id` # replicate ID (are the plate replicates of each other? (just easy for plotting options))
  locationID <- if(length(locationID) == 0) c("") else locationID
  plateID    <- if(length(plateID) == 0)    c("") else plateID
  imageID    <- if(length(imageID) == 0)    c("") else imageID
  timeID     <- if(length(timeID) == 0)     c("") else timeID
  replID     <- if(length(replID) == 0)     c("") else replID
  exposureDelay <- config$metadata$`exposure-delay` # hh:mm
  timeBetweenFrames <- config$metadata$`time-between-frame` # hh:mm:ss
  #define the paths of the measurements you are interested in, leave empty if you dont need so many: full full full empty empty empty.....  NOT: full empty full.....
  # this is for measurements: tracking will be handled automatically

  # only object related data or image related data( e.g. not implemented yet for Relationship/ Experiment related data )
  # only add the object/feature part for example:  "myObject/Inensity_MeanIntensity_img"

  myFeaturePathsA <- as.list(config$metadata$features)
  # tab delimted text file with metadata headers:    well  treatment  dose_uM	control	cell_line
  #the control is 1 or `1 where `1 is a control (just used for some extra coloring in plots)
  plateMDFileName <- config$input$layout

  parentObject <- config$metadata$objects$`parent-object`


  child.objects <- config$metadata$objects$`child-objects`
  stopifnot(length(child.objects) < 6)

  for(i in 1:5) {
    assign(paste("childObject", i, sep = ""), if(length(child.objects) >= i) child.objects[[i]] else NULL)
  }
  tertiaryObject <- config$metadata$objects$`tertiary-object` #  child of parentObject and childObject1 object. Defined (in CP) by substraction of larger object minus smaller object

  # what summary statistic do you prefer to display the multiple objects per parent object with? This is NOT performed for nuclei, but for children objects like foci this can be usefull
  multiplePerParentFunction <- function(x) { mean(x, na.rm = TRUE) }  # or  function(x) { quantile(x, 0.8, na.rm = TRUE) }

  oscillation <- FALSE  # TRUE / FALSE  - will extract oscillation related parameters of divisionOne (TRUE is not implemented yet)

  writeSingleCellDataPerWell <- FALSE # write all single cell data in seperate file per well, takes time
  writeAllSingleCellData<- FALSE  # Only needed of you need the txt file yourselfl writes all single cell data in single txt file.
  numberCores <- min(cores, detectCores()) # should be specified after sourcing mainFunction -> weird dependency, should fix


  #do the same for set2 if exists, then rbind the results with myDF
  outputList= list()
  # run main function: this could be made parralel but so far speed has not been an issue for this function
  if(length(hdf5FileNameL) > 1) {
    registerDoParallel(min(numberCores, length(hdf5FileNameL)))

    outputList<- foreach( h5loop = seq_along(hdf5FileNameL ),
                          .packages = c("rhdf5", "stringr", "data.table", "plyr")) %dopar%
                          {

                            mainFunction( h5loop=h5loop,
                                          hdf5FileNameL=hdf5FileNameL,locationID=locationID, timeID=timeID, plateID=plateID,
                                          imageID=imageID, replID = replID,
                                          myFeaturePathsA=myFeaturePathsA, plateMDFileName=plateMDFileName,
                                          parentObject=parentObject, childObject1=childObject1, childObject2=childObject2,
                                          childObject3=childObject3, childObject4=childObject4, childObject5=childObject5,
                                          tertiaryObject=tertiaryObject, multiplePerParentFunction=multiplePerParentFunction,
                                          oscillation=oscillation,
                                          writeSingleCellDataPerWell=writeSingleCellDataPerWell,
                                          writeAllSingleCellData=writeAllSingleCellData,
                                          timeBetweenFrames=timeBetweenFrames, exposureDelay=exposureDelay,
                                          numberCores = numberCores
                            )
                          }
  } else {
    h5loop <- 1
    outputList <- mainFunction( h5loop=h5loop,
                                hdf5FileNameL=hdf5FileNameL,locationID=locationID, timeID=timeID, plateID=plateID,
                                imageID=imageID, replID = replID,
                                myFeaturePathsA=myFeaturePathsA, plateMDFileName=plateMDFileName,
                                parentObject=parentObject, childObject1=childObject1, childObject2=childObject2,
                                childObject3=childObject3, childObject4=childObject4, childObject5=childObject5,
                                tertiaryObject=tertiaryObject, multiplePerParentFunction=multiplePerParentFunction,
                                oscillation=oscillation,
                                writeSingleCellDataPerWell=writeSingleCellDataPerWell,
                                writeAllSingleCellData=writeAllSingleCellData,
                                timeBetweenFrames=timeBetweenFrames, exposureDelay =exposureDelay,
                                numberCores = numberCores
    )
  }


  save(outputList, file = 'outputList.Rdata')
}









# BLOCK 4: Tracking block.  For analyzing migration related data, run block1 and then block 4
# This block is meant to reorganize data suitable for tracking -  CP does still not relabel tracked objects after splits or merges.
# Also some options of reconnecting tracks included
trackingFun <- function(config, cores) {

  load('outputList.Rdata')

  if(length(unlist(lapply( lapply(outputList, names),str_match_all, "myDT") )) > 1) {
    outputListmyDT<- lapply(outputList, "[[", "myDT")
    testColN<- lapply(outputListmyDT, function(x) {(  (names(x)))} )
    all.identical <- function(x) all(mapply(identical, x[1], x[-1]))
    if(!all.identical(testColN))
    {
      stop("outputlist does have tables with identical column names/
             object names, manually rbind the outputlist")
    }

    myDFo <- rbindlist(outputListmyDT)


    kMyVars <- outputList[length(outputList)][[1]]
    kMyVars$myDT <- NULL
  } else {
    outputListmyDT <- outputList$myDT
    myDFo <- outputListmyDT
    kMyVars <- outputList[-1]
  }


  kColNames <- kMyVars$kColNames
  dataFileName <- gsub(".txt", "",kMyVars$plateMDFileName)

  myFeatures <- gsub("/", "_",
                     gsub("^(Measurements/[0-9]{4}(-[0-9]{2}){5}/)", "", kMyVars$myFeaturePathsA)
  )
  myFeatures <- c(myFeatures, "imageCountTracked")
  numberCores <- kMyVars$numberCores
  writeSingleCellDataPerWell <- TRUE # write all single cell data in seperate file per well, takes time
  writeAllSingleCellData<- TRUE  # write all single cell data in single file, takes time, usefull because plotting/ summary chunk can then load this for later (re) runs



  # ===================== User defined variables =====================
  # ===================== User defined variables =====================
  # ===================== User defined variables =====================


  reconnect_tracks <- config$`reconnect-tracks` # moet op true (FALSE not implemented yet)
  max_pixel_reconnect1 <- config$`max-pixel-reconnect`[1] # if larger than CP settings calculation overhead can become alot higher
  max_pixel_reconnect2 <- config$`max-pixel-reconnect`[2] # further in time cells might be further away from parent
  max_pixel_reconnect3 <- config$`max-pixel-reconnect`[3]
  reconnect_frames <- config$`reconnect-over-frames`  # over how many frames to connect? can choose 1,2 or 3. 1 means no frame is skipped, 2 then 1 frame is skipped etc. will first perform direct linking then skip 1 frame then 2 to try and reconnect tracks based on maximal considered distance
  skip.wells <- c( )
  minTrackedFrames <- config$`min-tracked-frames` # remove short tracks from data output
  parent_resolve_strategy <- config$`parent-resolve-strategy` # how to resolve duplicate parents

  summaryStatFunction <- function(x) { mean(x, na.rm = TRUE) } # function(x) { mean(x, na.rm = TRUE) }  or function(x) { quantile(x, 0.8, na.rm = TRUE) } (you can choose which quantile - here it is set to 0.8)
  errorType <- "sd"   #"sd"  or  "cl95"   the cl95 is two sided 95% confidence interval. sd is standard error, half above and half under the average
  numberCores <- cores
  writeUniqueParentsNoRec <- config$write$`unique-parents-no-rec`
  writeBeforeCombineTracks<- config$write$`before-combine-tracks`
  writeAfterFirstConnect<- config$write$`after-first-connect`
  writeAfterSecondReconnect<- config$write$`after-second-connect`
  writeAfterThirdReconnect<- config$write$`after-third-connect`
  ## ========================== end user defined variables============
  ## ========================== end user defined variables============
  ## ========================== end user defined variables============
  ## ========================== end user defined variables============



  if(exists('allTrackDF')){
    rm(allTrackDF) # needs to be removed to be able to re-run this block
  }



  # get all the features needed for plotting:
  myFeature <- myFeatures[1]


  # if this is enabled the myDFo has to be modified so that no NA values exist for the tracked object. Assumed is that if an NA exists, the row  corresponding to a certain object will be NA

  Parent_NON <- outputList$kColNames$parentObjectNumberCN
  myDT <- myDFo
  if (sum(is.na(myDT[,Parent_NON, with = FALSE])) > 0 )
  {
    stop("NA values in dataset. Consider using a different CP pipeline")
  }
  # if needed I might have to remove certain the rows with certain column specific NA values (the measurements for example, or maybe the x-y coordinates?)

  colnames(myDT)
  uniqueWells <- unique(myDT[,locationID])
  uniqueWells <- uniqueWells[ !uniqueWells %in% skip.wells]
  uniqueWells<- factor(uniqueWells)

  # check cell count
  #cl<-makeCluster(1)
  # registerDoSNOW(cl)

  #split data in numberCore parts, if length(uniqueWells) > numberCores
  if( length(uniqueWells) < numberCores) {
    stop("Reduce the number of cores")
  }

  jumpInd <-length(uniqueWells) %/% numberCores
  uniqueWellsLevels <- rep(1:numberCores, each = jumpInd)
  #add some extra at the end in case levels is shorter:
  extraEnd <-  length(uniqueWells) - length(uniqueWellsLevels)
  uniqueWellsLevels<- c(uniqueWellsLevels,  rep(uniqueWellsLevels[length(uniqueWellsLevels)], extraEnd))
  if(length(uniqueWellsLevels) != length(uniqueWells) | !all(sort(uniqueWellsLevels) == uniqueWellsLevels)) {
    stop("making uniqueWellLevels failed")
  }
  uniqueWellGroups  = list()
  for(countergroups in seq_along(unique(uniqueWellsLevels))) {
    uniqueWellGroups[[countergroups]] <- uniqueWells[ uniqueWellsLevels == countergroups]
  }

  registerDoParallel(cores=numberCores)

  cellNlist <- foreach ( cellC = seq_along(1:numberCores ), .packages = 'data.table') %dopar% {

    ind <-   myDT[ , locationID] %in% uniqueWellGroups[[cellC]]
    partmyDT <- myDT[ind,]
    out.min <- partmyDT[, min(imageCountTracked), by = locationID]
    out.max <- partmyDT[, max(imageCountTracked), by = locationID]
    setnames(out.min, 'V1', "minimum number of tracked objects")
    setnames(out.max, 'V1', "maximum number of tracked objects")
    setkey(out.min,locationID)
    setkey(out.max,locationID)
    out.both <- out.min[out.max]
    out.both
  }
  cellNlist <- rbind.fill(cellNlist)
  write.table(cellNlist, file = 'trackedObject_counts.txt' ,  row.names = FALSE, sep = "\t")
  #rm("out.min", "out.max","out.both", "cellNlist", "partmyDFo")
  #selFeatures <- gsub("\\/", "_",  str_match(  unlist(myFeaturePathsA), "([^/]*[\\/][.]*[^/]*)$")[, 1 ] )

  # selFeatures <- selFeatures[!is.na(selFeatures)]

  setkey(myDT, locationID)

  #TODO: comment this away/ remove this once not needed anymore
  # will need to fix some colnames from wrong mainFunction.R output before 9 dec 2014
  indC<-grep(  "_TrackObjects_DistanceTraveled_",colnames(myDT))

  track_dist <- str_match(colnames(myDT)[indC],"(DistanceTraveled_[0-9]{1,3})$" )[2]
  track_dist<- gsub("DistanceTraveled_", "", track_dist)
  indEnd_ <- unlist(lapply(kColNames, function(x) {grepl("(_)$", x)  }))
  names(indEnd_) <- NULL
  kColNames[indEnd_] <- paste(kColNames[indEnd_], track_dist, sep="")

  registerDoParallel(cores=numberCores)



  allTrackDF<-foreach(i = seq_along(uniqueWellGroups), .export=c("fixTrackingFun"),
                      .packages = c("reshape2", "plyr","stringr", "data.table" )) %dopar% {
                        myDFstukkie <- myDT[ uniqueWellGroups[ i ]]
                        fixTrackingFun(myDFstukkie, myFeatures, i, kColNames, uniqueWellGroups,
                                       writeUniqueParentsNoRec, writeBeforeCombineTracks, reconnect_tracks,
                                       max_pixel_reconnect1, max_pixel_reconnect2, max_pixel_reconnect3,
                                       writeAfterFirstConnect, writeAfterSecondReconnect, writeAfterThirdReconnect,
                                       reconnect_frames, minTrackedFrames, writeSingleCellDataPerWell, parent_resolve_strategy)
                      }



  # pull out distinct data sets

  allTrackDFreal <- lapply(allTrackDF, '[[', "allTrackDF")
  directionality.data <- lapply(allTrackDF, '[[', "directionality.data")
  directionality.data <- rbind.fill(directionality.data)
  allTrackDF <- rbind.fill(allTrackDFreal)
  rm(allTrackDFreal)

  directionality.data <- as.data.table(directionality.data)
  allTrackDF <- as.data.frame(allTrackDF)

  #dit van RData file halen

  metaData <- kMyVars$metaCSVData
  indRM <- which(unlist(lapply(metaData, function(x) any(is.na(x)))))
  print(c("removing columns from metadata file:" ,indRM))
  if(!length(indRM) < 1) {
    metaData[, eval(indRM):= NULL]
  }


  #allTrackDF$treatment <- NA
  #allTrackDF$dose_uM <- NA
  #allTrackDF$control <- NA
  #allTrackDF$cell_line <- NA
  print("adding metadata:")

  head(allTrackDF$location)
  allTrackDF <- as.data.table(allTrackDF)
  allTrackDF[, mergeLocation:= gsub("(_[1-9]{1})$", "", location)]

  allTrackDF[ , mergeLocation:= as.factor(mergeLocation)]
  metaData[, locationID:= as.factor(locationID)]
  setkey(allTrackDF, "mergeLocation")
  setkey(metaData, "locationID")

  allTrackDF<- metaData[allTrackDF]


  #allTrackDF <- allTrackDF[ !is.na(allTrackDF$value), ] # dit gaat niet ivm verschil in NA voor bijv eerste tijdpunt displacement
  # misschien adv bepaalde variabele de na indexen maken en dan per feature deze index gebruiken
  # niet ideaal om hier te doen (memory/ performance), maar voorlopig:
  zehFeats <- unique(allTrackDF$.id)

  # first feature for index

  print("use first feature that is not displacement to create index to remove NA values from all features")
  ind <- !is.na(allTrackDF[ allTrackDF$.id == myFeatures[!myFeatures %in% c("displacement" ) ][1], value ])
  head(allTrackDF)

  bufferList = list()
  for( i in seq_along(zehFeats)){

    bufferList[[i]] <- allTrackDF[  allTrackDF$.id == zehFeats[i], ][ind,]
  }
  bufferDF <- rbind.fill(bufferList)

  write.table(bufferDF, file = "reorderedTrackData.txt", sep ="\t", row.names = FALSE)
  write.table(directionality.data, file = "directionality.txt", sep ="\t", row.names= FALSE)
}
