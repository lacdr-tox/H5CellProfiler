
mainFunction <- function(  hdf5FileName,  locationID, imageID, timeID, plateID,  myFeaturePathsA, plateMDFileName, 
                           parentObject, childObject1, childObject2, childObject3, childObject4, childObject5, tertiaryObject,
                          multiplePerParentFunction, thresholdMaxSecChildren, divisionOne, divisionTwo, kAddDenom, kRmMax, binaryOne, binaryTwo, 
                          oscillation, writeSingleCellDataPerWell, writeAllSingleCellData, h5loop) {
  h5loop=1
  source("C:/Users/stevenadmin/work/scripts/CPHDF5Analysis/verifyInputFun.R")
  hdf5FileName <- hdf5FileName[[h5loop]]
  
  # some small functions used later on:
  
  veryfInput <- function(input) {
    varname <- deparse(substitute(input))
    out="nothing";
    if (exists(varname)) {out="input exists";} else {stop("input does not exist")}
    return(out);
  }
  
  
  
  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }
  is.character0 <- function(x)
  {
    is.character(x) && length(x) == 0L
  }
  
  
  # pull out needed path / object data
  all.paths <- h5ls(hdf5FileName)$group
  all.dataID <-unique(str_extract(all.paths ,  "/[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}/"))
  all.dataID<- all.dataID[ !is.na(all.dataID)]
  setDateID<- gsub("/", "", all.dataID)
  if(nchar(setDateID) != 19) {
    stop("setDateID reg expr failed")
  }
  
  ind.tr<-grepl("TrackObjects", all.paths)
  all.paths.tr<-all.paths[ ind.tr]
  # perl "look behind" ?<=
  rmIndIm<- grepl("Image", all.paths.tr)
  all.paths.tr<-all.paths.tr[!rmIndIm]
  trackedObject <- unique(str_extract(all.paths.tr, 
                                      perl('(?<=Measurements/[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}/)[A-Z a-z _ 0-9]+(?=/)')))
  
myFeaturePathsA <- paste( "Measurements", setDateID ,myFeaturePathsA, sep = "/")
  if( nchar(trackedObject) > 0 ){
    track_dist<-unique(str_extract(all.paths.tr, '[0-9]{1,3}$')  )
    if(length(track_dist) != 1){
      stop("reg expr for track_dist failed")
  }
  trackedObjectDisplacement<- paste("TrackObjects_DistanceTraveled", track_dist, sep ="_") 
  trackingLabel <- paste(trackedObject, "/", "TrackObjects_Label_", track_dist , sep  ="") 
  # dont confuse this with Parent object:
  trackingParent <- paste(trackedObject, "/", "TrackObjects_ParentObjectNumber_", track_dist, sep ="") 
  trackingLabel <- paste( "Measurements", setDateID, trackingLabel, sep = "/" )  
  xCoordPath <- paste("Measurements",  setDateID ,trackedObject,"Location_Center_X", sep = "/" )
  yCoordPath <- paste("Measurements",  setDateID ,trackedObject,"Location_Center_Y", sep = "/" )
  DistanceTraveledPath <- paste( "Measurements", setDateID, trackedObject, 
                                 trackedObjectDisplacement, sep = "/")
  trackingParent <- paste( "Measurements", setDateID, trackingParent, sep = "/" )
  myFeaturePathsA <- c(myFeaturePathsA, 
                       trackingLabel, 
                       trackingParent,
                       xCoordPath,
                       yCoordPath,
                       DistanceTraveledPath)
    
  
  }
myFeaturePathsA <- myFeaturePathsA[lapply(myFeaturePathsA, nchar) >33] # remove empty entries  
myFeature <- myFeaturePathsA[[1]]
 
  #variables of your metadata that you specified in CP. locationID must exist and be the image location eg A01_1
  locationID <- paste( "Measurements", setDateID, locationID, sep = "/" )
    
  if ( imageID != "")
  {
    imageID <- paste( "Measurements", setDateID, imageID, sep = "/" )
  }
  if ( timeID != "")
  {
    timeID <- paste( "Measurements", setDateID, timeID, sep = "/" )
  }
  
  
   
  
  # verify user input:
  my.objects <- c(parentObject,
                  childObject1,
                  childObject2,
                  childObject3,
                  childObject4,
                  childObject5,
                  tertiaryObject)
             # my.objects: defined by user to define parent/ childrens       
                  my.objects <- my.objects[ unlist(
                              lapply(my.objects, function(x) { nchar(x) > 0 })
                              )]
                                
  pat<-"([/][[:alnum:][:digit:][:punct:]]*[/])"   
 # all.objects defined from measurements and other fixed data
  all.objects <- unique( lapply(myFeaturePathsA, function(x) {
              y = gsub("/","",str_match(
                gsub(paste("Measurements/", setDateID, sep =""), "", x )
                , pat ))[1]
              } ))
verifyInputFun()

    
  # function that takes hdf5 list as input and returns correctly ordered df as output
  hdf5IndexFun <- function( hdf5Path, dataName, rowIndName ) {  
   
    veryfInput(input =dataName) 
    veryfInput(input = hdf5Path) 
   
    if( !is.character(hdf5Path) ) {
      stop("inputpath must be character")
    }
    checkRootpath <- str_extract(hdf5Path ,  "Measurements/[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}/")
    if(nchar(checkRootpath) != 33) {
      stop("Rootpath error")
    }
      
    
    hdf5List = h5read( hdf5FileName, hdf5Path,bit64conversion="bit64" )
    if(!is.list(hdf5List)){
      stop("Not of class list")
    }
    if(length(hdf5List) != 2 ){
      stop("List not length 2")
    }
    if(dim(hdf5List[[2]])[1]!= 3){
      stop("index does not consist of 3 vectors (imageNr, start row-index, end row-index")
    }
    if(dim(hdf5List[[1]])[1]< 1){
      stop("Empty data vector")
    }  
    
    index<-hdf5List[["index"]]
    index <- index[, index[3,] != 0] # some images do not have objects, sometimes this is 0, sometimes equal numbers
    index <- index[ , index[2,] != index[3,] ]
    index<-t(index)
    indexList <- apply(index[, -1], MARGIN = 1, function(x) seq(x[1]+1, x[2]))
    names(indexList) <- index[,1]
    stackedIndex <- stack(indexList)
    colnames(stackedIndex) <- c("rowInd", "imageInd")
    
      if(length(hdf5List[["data"]]) != nrow(stackedIndex) ){
        stop("stacking error")
      }
      # add index to data:
    outDF <- data.table(imageNumber = stackedIndex$imageInd[stackedIndex$rowInd] ,
                        dataOut     = hdf5List[["data"]],
                        rowInd = stackedIndex$rowInd[stackedIndex$rowInd],
                        key = "imageNumber"
                        )
        
    setnames(outDF,"dataOut",dataName)
    setnames(outDF,"rowInd", paste(rowIndName, "_rowInd", sep = ""))
        
    return(outDF)
  } # "rowInd", "imageInd" are keys ( $sorted )
  
    
  # A small test, probably deletable:
          imageParentObjIndPath <- paste( "Measurements", setDateID, parentObject, "ImageNumber", sep = "/" ) #no need to merge this
        dataName <- "imageParentObjInd"
      hdf5Path <- imageParentObjIndPath
    rowIndName <- parentObject
  imageParentObjInd<- hdf5IndexFun(hdf5Path = hdf5Path , dataName = dataName, rowIndName = rowIndName)
if( !all(imageParentObjInd$imageNumber == imageParentObjInd$imageParentObjInd)){
  stop("stacking problem imageParentObjInd =! imageNumber")
}


    # all fixed image data needed ( includes group and group index data because CellProfiler should have each location defined as a group - even for single time points:
    # 
    
          imageCountTrackedPath <- paste( "Measurements", setDateID, "Image", 
                                          paste( "Count", trackedObject, sep ="_" ), sep = "/" )
        dataName <- "imageCountTracked"
      hdf5Path <- imageCountTrackedPath
    rowIndName <- "image"
  imageCountTracked <- hdf5IndexFun( hdf5Path = hdf5Path , dataName = dataName, rowIndName = rowIndName )

  groupIndPath <- paste( "Measurements", setDateID, "Image/Group_Index", sep = "/" ) # within group number (usually time point)
    dataName <- "groupInd"
      hdf5Path <- groupIndPath
        rowIndName <- "image"
          groupInd <- hdf5IndexFun( hdf5Path = hdf5Path , dataName = dataName, rowIndName = rowIndName )
    
          groupNumberPath <- paste( "Measurements", setDateID, "Image/Group_Number", sep = "/" ) # the group number (usually location number)
        dataName <- "groupNumber"
      hdf5Path <- groupNumberPath
    rowIndName <- "image"
  groupNumber <- hdf5IndexFun( hdf5Path = hdf5Path , dataName = dataName, rowIndName = rowIndName )


myDFrawImage1 <- groupInd[groupNumber]
myDFrawImage <- myDFrawImage1[imageCountTracked]


rm( 'myDFrawImage1', 'groupInd', 'groupNumber', 'imageCountTracked')

# add metadata. Once for all the hdf5-path based and once for all manually defined metadata
metaDataList <- c(locationID[h5loop], imageID[h5loop], timeID[h5loop], plateID[h5loop], replID[h5loop])
names(metaDataList) <- c("locationID", "imageID", "timeID", "plateID", "replID")
metaDataList <- metaDataList[ lapply(metaDataList, nchar) > 0]
indh5Paths <- grepl('/',metaDataList)
if(!indh5Paths[1]){
  stop("well metadata must be h5 path")
}

for( mloop in seq_along(metaDataList)){
    if(indh5Paths[mloop]) {
        hdf5Path <- metaDataList[[mloop]]
        dataName <- names(metaDataList)[mloop]        
        rowIndName <- "image"
        currImageDatatmp <- hdf5IndexFun( hdf5Path = hdf5Path , 
                                                dataName = dataName, 
                                                rowIndName = rowIndName )
          if(!exists('currImageData')){
          currImageData <- currImageDatatmp
          } else {
              currImageData <- merge(currImageData, currImageDatatmp)
              }
        } else # if not hdf5 path 
            {
            currImageData$buffer <-  metaDataList[[mloop]]
            setnames( currImageData, 'buffer', names(metaDataList)[[mloop]]  )
      }
}

# merge to myDFrawImage
myDFrawImage <-myDFrawImage[currImageData]

# add metadata from csv file to imageDF

metaCSVData <- fread( input =plateMDFileName, sep = "\t", header = TRUE,  verbose = T )
setkey(metaCSVData, 'treatment')

# only use data that is defined in metadata layout file, in treatment column
metaCSVData <- metaCSVData[ !"" ]
metaCSVData <- metaCSVData[ !"0" ]
metaCSVData <- metaCSVData[ !0 ]
metaCSVData <- metaCSVData[ !is.na(locationID),]

if ( length(metaCSVData$treatment) != length(metaCSVData$dose_uM[metaCSVData$dose_uM!="" & !is.na(metaCSVData$dose_uM)]) ){
  stop("Some treatments did not have valid concentration in CSV metadata file")
}

HDF5notInLayout <- !gsub( "(_[0-9]{1,2})$", "", unique(myDFrawImage[, locationID] )) %in% 
  unique(metaCSVData$locationID)

layoutnotInHDF5 <- !unique(metaCSVData$locationID) %in% 
  gsub( "(_[0-9]{1,2})$", "", unique(myDFrawImage[, locationID] )) 

write.table(file = "HDF5notInLayout.txt", 
            gsub( "(_[0-9]{1,2})$", "", 
                  unique(myDFrawImage[, locationID] ))[HDF5notInLayout], sep = "\t"  )
write.table(file =  "layoutnotInHDF5.txt", unique(metaCSVData$locationID)[layoutnotInHDF5], sep = "\t" )

ind <- match(  gsub( "(_[0-9]{1,2})$", "", myDFrawImage[, locationID] ), metaCSVData$locationID)
  if ( length( ind ) != sum(!is.na(ind) ) | length( ind ) == 0 ) 
      {
      warning( "CP analysed wells not found, consult written text files")
      }
myDFrawImage$joiner <- gsub( "(_[0-9]{1,2})$", "", myDFrawImage[ , locationID ])
setkey(myDFrawImage, "joiner" )
setkey(metaCSVData, 'locationID')
myDFImage <- metaCSVData[myDFrawImage]

  if(!identical(myDFImage$locationID, myDFImage$locationID.1)){
    stop("merge metadata and image data failed")
  }
myDFImage$locationID.1 <- NULL

# select correct metadata columns (from hdf5, manually or from metadata layout file)
# if NA in metadata layout then these are removed. Else alternative is removed and metadata ones renamed
if(any(is.na(myDFImage[ , plateID]))) {
  myDFImage$plateID <- NULL
  setnames(myDFImage, 'plateID.1', 'plateID')
} else {
    myDFImage$plateID.1 <- NULL
}
if(any(is.na(myDFImage[ , timeID]))) {
  myDFImage$timeID <- NULL
  setnames(myDFImage, 'timeID.1', 'timeID')
} else {
  myDFImage$timeID.1 <- NULL
}
if(any(is.na(myDFImage[ , replID]))) {
  myDFImage$replID <- NULL
  setnames(myDFImage, 'replID.1', 'replID')
} else {
  myDFImage$replID.1 <- NULL
}


# To pass an expression into your own function, one idiom is as follows :
#   > DT = as.data.table(iris)
# > setkey(DT,Species)
# > myfunction = function(dt, expr) {
#   + e = substitute(expr)
#   + dt[,eval(e),by=Species]
#   + }
# > myfunction(DT,sum(Sepal.Width))

if(nrow(myDFImage) != nrow(myDFrawImage)){
  warning("merging metadata error: some CP-analysed data not found in plate-lyaout file, consult written text file" )
  }

if(any(is.na(myDFImage$imageNumber))){
  stop("missing data in metadata file")
}
rm("myDFrawImage")  
  #=====
  #remove two levels from "feature"- path, use this for other paths in h5 file
  #exprPathI <- gsub( "([/][^/]*)$", "", firstFeature  )
  #=====
  
  #now add all info to the corresponding dataframes using regular expressions of the ( if exists ) 4 object definitions + the image path
# pleur alles in myFeaturePaths, combineer alles per object, voeg indexen toe:

# TODO: hier gebleven in de VAKAANTIE

myFeaturePaths <- myFeaturePathsA[ myFeaturePathsA != paste( "Measurements", setDateID, "", sep = "/" )  ]
getParenObj <- my.objects[my.objects!=parentObject]  # children objects parent paths van vinden
if( !is.character0(getParenObj)  ) 
  {
  getParenObjPath <- paste( "Measurements/", setDateID, "/", getParenObj, "/Parent_", parentObject, sep = '')
  myFeaturePaths <- c(myFeaturePaths, getParenObjPath)
  }

# add number_object_number of parent object
numObNumPath <- paste( "Measurements/", setDateID, "/", parentObject, "/Number_Object_Number", sep = "")
myFeaturePaths <- c(myFeaturePaths, numObNumPath)
  myFeaturesData = list()   
if ( ( length(myFeaturePaths) != 0) ) {
  for ( i in 1 : length( myFeaturePaths ) ) 
    {
    hdf5Path <- myFeaturePaths[[i]]
      preName <- gsub(paste("Measurements/", setDateID, sep =""), "", myFeaturePaths[[i]])
      pat<-"([/][[:alnum:][:digit:][:punct:]]*[/])" 
    rowIndName <- gsub("/","",str_match(preName, pat ))[1]
    dataName <- gsub(rowIndName, "", preName) 
    dataName <- gsub("/", "", dataName)
    myFeaturesData[[i]] <- hdf5IndexFun( hdf5Path = hdf5Path , 
                                      dataName = dataName, 
                                      rowIndName = rowIndName )
    
  names(myFeaturesData)[i] <- rowIndName  
  }
    }
# For each object type, create a data.table

# parent:
parentInd <- parentObject == names(myFeaturesData)
myDTParentList <- myFeaturesData[parentInd]

objName <- unique(names(myDTParentList))
objName <- paste(objName, 'rowInd', sep = '_')
myDTParentList <- lapply(myDTParentList, function(x) x<- setkeyv(x, objName))
myDTParent <- myDTParentList[[1]]


if(length(myDTParentList) > 1 ){
    
  for (cbloop in 1: (length(myDTParentList)-1) ) {
    myDTParent <- myDTParent[myDTParentList[[cbloop+1]]]
  }
}
rm("myDTParentList")
hier gebeleven
grepl imagenumber..
all(identical...








  #Parent object data
  if ( sum(indParent) != 0) {
    for ( i in ( 1 : length( myFeaturePaths) )[ indParent ] )
    { 
      featureName <- gsub("\\/", "_",  str_match(  myFeaturePaths[[ i ]], "([^/]*[\\/][.]*[^/]*)$" )[ 1 ] ) 
      myDFrawParent$dummy <- myFeaturesData[[ i ]]
      colnames( myDFrawParent )[ length( colnames( myDFrawParent ) ) ] <- featureName
    }
    
  }
  
# all fixed parent object data needed:
parentObjectNumberPath <- paste( "Measurements", setDateID, parentObject, "Number_Object_Number", sep = "/" )
dataName <- "parentObjectNumber"
hdf5Path<- parentObjectNumberPath
rowIndName <- parentObject
parentObjectNumber<- hdf5IndexFun( hdf5Path = hdf5Path, dataName = dataName, rowIndName = rowIndName )
parentObjectNumber

  
  #secondary object data
  #first create secondary data dataframe:
  
  # function that creates child and tertiary object-data.frames
  
  
  
  
  createChildDF <- function(){
    
    childDF_list = list()
    
    for (childCounter in seq_along( zeh.objects)){
      
      currentPathImageNumber <- paste( "Measurements", setDateID, zeh.objects[childCounter], "ImageNumber", sep = "/" )
      suppressWarnings(currentImageNumberData<-  h5read( hdf5FileName, currentPathImageNumber ))
      if(!all(currentImageNumberData[["index"]][1,]== crossCheckImageIndexWithAllObjects  )){
        stop(paste("currentImageNumberData at childCounter", childCounter, "could not be crossCheckImageIndex validated"))
      }
      childDF_list[[ childCounter ]] <- data.frame( buffer = as.integer(currentImageNumberData[[ "data" ]]))
      colnames(childDF_list[[ childCounter ]]) <- paste("image", zeh.objects.names[ childCounter ], "Ind", sep = '' )  
      
      currentParentObjectPath <- paste( "Measurements", setDateID, zeh.objects[childCounter], paste( "Parent", parentObject, sep = "_" ), sep = "/"  )
      suppressWarnings(currentParentObjectData <- h5read( hdf5FileName, currentParentObjectPath ))
      if(!all(currentParentObjectData[["index"]][1,] == crossCheckImageIndexWithAllObjects )){
        stop(paste("currentParentObjectData at childCounter", childCounter, "could not be crossCheckImageIndex validated"))
      }
      childDF_list[[ childCounter ]]$buffer <- as.integer(currentParentObjectData[[ "data" ]])
      colnames(childDF_list[[ childCounter]])[2] <- paste( "ParentObjOf", zeh.objects.names[childCounter], sep ='')
      
      
      if ( sum(zeh.indexes[[childCounter]]) > 0 ) {# then user defined secondary/tert object data
        #ind first match to determine length: 
        
        for ( i in (1 : length( myFeaturePaths ) )[ zeh.indexes[[childCounter]] ] )
        { 
          featureName <- gsub("\\/", "_",  str_match(  myFeaturePaths[[ i ]], "([^/]*[\\/][.]*[^/]*)$")[ 1 ] ) 
          
          childDF_list[[ childCounter ]]$dummy <-  myFeaturesData[[ i ]] 
          colnames( childDF_list[[ childCounter ]] )[ length( colnames( childDF_list[[ childCounter ]] ) ) ] <- featureName
        }
      }
      
      
    } #childCounter loop
    
    rm(currentImageNumberData, currentParentObjectData)
    return(childDF_list)
  } # end createFD function
  
  # run the function
  if(nchar(childObject1)>0){
    ChildDF<- createChildDF()
  }
  #define seperate data.frames
  
  
  
  # image data
  if ( sum(indImage) != 0) {
    for ( i in (1 : length( myFeaturePaths))[ indImage ] )
    { 
      featureName <- gsub("\\/", "_",  str_match(  myFeaturePaths[[ i ]], "([^/]*[\\/][.]*[^/]*)$")[ 1 ] ) 
      myDFrawImage$dummy <- myFeaturesData[[ i ]]
      colnames( myDFrawImage )[ length( colnames( myDFrawImage ) ) ] <- featureName
    }
  }
  
  rm("myFeaturesData")
  
  
  # now add tracking data ( if exists ) to the correct dataframe
  if (  trackedObject  != "" ) # only makes sense if tracking was enabled
  {
    # load fixed tracked-object- length related data
    
    parentObjectNumberPath <- paste( "Measurements", setDateID, parentObject, "Number_Object_Number", sep = "/" )
    trackedObjectNumberPath <- paste("Measurements", setDateID, trackedObject, "Number_Object_Number", sep = "/")  
    xCoordPath <- paste("Measurements",  setDateID ,trackedObject,"Location_Center_X", sep = "/" )
    yCoordPath <- paste("Measurements",  setDateID ,trackedObject,"Location_Center_Y", sep = "/" )
    
    
    
    
    trackObj <- h5read( hdf5FileName, trackingLabel )
    trackObj2 <- h5read( hdf5FileName, trackingParent)
    Displacement <- h5read( hdf5FileName, paste( "Measurements", setDateID, trackedObject, trackedObjectDisplacement, sep = "/") )
    trackedObjectNumber <- h5read( hdf5FileName, trackedObjectNumberPath)
    xCoord<- h5read( hdf5FileName, xCoordPath )
    yCoord<- h5read( hdf5FileName, yCoordPath )
    
    if( !all(trackObj[["index"]][1,] == crossCheckImageIndexWithAllObjects) |
          !all(trackObj2[["index"]][1,] == crossCheckImageIndexWithAllObjects) |
          !all(Displacement[["index"]][1,] == crossCheckImageIndexWithAllObjects) |
          !all(trackedObjectNumber[["index"]][1,] == crossCheckImageIndexWithAllObjects) |
          !all(xCoord[["index"]][1,] == crossCheckImageIndexWithAllObjects) |
          !all(yCoord[["index"]][1,] == crossCheckImageIndexWithAllObjects)
    ) {
      stop("Tracking related data did not pass crossCheckImage validation")
    }
    
    trackObj <- as.integer(trackObj[["data"]])
    trackObj2 <- as.integer(trackObj2[["data"]])
    Displacement <- as.integer(Displacement[["data"]])
    trackedObjectNumber <- as.integer(trackedObjectNumber[["data"]])
    xCoord<- as.numeric(round(xCoord[["data"]], digits = 1))
    yCoord<- as.numeric(round(yCoord[["data"]], digits = 1))
    
    
    if ( trackedObject == parentObject )
    {
      myDFrawParent$trackObjectsLabel = trackObj
      myDFrawParent$trackObjectParent = trackObj2
      myDFrawParent$Displacement = Displacement
      myDFrawParent$trackedObjectNumber = trackedObjectNumber
      myDFrawParent$xCoord = xCoord
      myDFrawParent$yCoord = yCoord
    } else if ( trackedObject == childObject1 )
    {
      
      ind <- which(zeh.objects.names=="ChildObject1") 
      
      
      ChildDF[[ind]]$trackObjectsLabel <- trackObj
      ChildDF[[ind]]$trackObjectParent<- trackObj2
      ChildDF[[ind]]$Displacement <- Displacement
      ChildDF[[ind]]$trackedObjectNumber <- trackedObjectNumber
      ChildDF[[ind]]$xCoord <- xCoord
      ChildDF[[ind]]$yCoord <- yCoord
      
      
    } else 
    {
      
      stop("trackObject must be defined either as a parent object or childObject1")
    }
    
    rm( "trackObj", "trackObj2", "Displacement", "trackedObjectNumber", "xCoord", "yCoord")
    
  }
  
  
  # find out which dataframe has multiple objects per tracked object,  objects are assumed to always have a defined parent/  child relationship since the relationship module must be run in CellProfiler to be able to link related objects
  
  # this could be done comparing nrows of dataframes but safer is to determine the chidren_count in parent object
  # the tracked object is gold standard here, summary statistics will be taken of other objects, also if the tracked object is not the parent. 
  # take note of the following:
  # the parent object will be the encompassing object (often biggest) of the non tertiary objects. 
  
  # Goal is now to not mess with the nuclei data, so some data entries will contain a nucleus that has no parent cytosol for example.
  
  # Tracked object can be the parent or a child object
  # if tracked object is a parent: remove child objects that have no parent & calculate summary statistic of children that have the same parent
  # if tracked object is a child: for each tracked object/ child find the parent (each child has 1 or 0 parents : definition-> parent can have multiple children, child can not have multiple parents) - therefore it can be that some parent have no child, and also a child has no parent
  
  if ( parentObject == trackedObject & nchar(trackedObject) > 0)  {  # if tracked object is also parent object
    
    # just an extra if-block in the case only 1 object was defined and is thus the parent object and the tracked object
    
    if( nchar( childObject1 ) == 0 & nchar( childObject2 ) == 0 &
          nchar( childObject3 ) == 0 & nchar( childObject4 ) == 0 & nchar( childObject5 ) == 0  ) {
      myDFmain <- myDFrawParent
    }
    
    combineObjectsFun <- function() {
      
      for(combCount in seq_along(zeh.objects)){
        
        ind <- which(zeh.objects.names==zeh.objects.names[combCount]) 
        ChildDF[[ind]] <- ChildDF[[ind]][ ChildDF[[ind]][paste("ParentObjOf", zeh.objects.names[combCount], sep = '')] != 0, ] # remove sec obj with no parent
        
        maxSecChildrenPath  <- paste( "Measurements", setDateID, parentObject, paste( "Children", zeh.objects[combCount], "Count", sep = "_" ), sep ="/" ) 
        suppressWarnings(SecChildren <-  as.integer(h5read( hdf5FileName, maxSecChildrenPath )[[ "data" ]] ))
        fractionMultiple <- sum(SecChildren > 1) / length(SecChildren)
        rm("SecChildren")
        
        if ( fractionMultiple > thresholdMaxSecChildren  )
        {
          # now within each image, find which objects have identical parent and calculate summary statistic
          #this is a bi*** on computation time ( 50 secs  )
          groupVars <- c( paste("image", zeh.objects.names[combCount], "Ind", sep =''), paste("ParentObjOf", zeh.objects.names[combCount], sep = ''))
          secVars <- colnames(ChildDF[[ind]])[ !(colnames(ChildDF[[ind]]) %in% groupVars) ]
          if (length(colnames(ChildDF[[ind]])) -2 != length(secVars)) {
            stop("likely the groupVars for secondary were not found")
          }
          nRows <- nrow(ChildDF[[ind]])
          buffer<-aggregate(x= ChildDF[[ind]][, secVars],by= ChildDF[[ind]][, groupVars ],  multiplePerParentFunction  ) # can take long 
          ChildDF[[ind]] <- buffer
          colInd <- which(groupVars %in% colnames(ChildDF[[ind]]))
          colnames(ChildDF[[ind]])[-colInd] <- secVars 
          if ( nRows <= nrow(ChildDF[[ind]])){
            stop( "not multiple secondary per parent or aggregration problem")
          }
          
        }
        if(!exists("myDFmain")){
          max(ChildDF[[ind]]$imageChildObject1Ind)
          
          dim(myDFrawParent)
          myDFmain <- merge( myDFrawParent, ChildDF[[ind]], by.x = c("parentObjectNumber", "imageParentObjInd"),
                             by.y = c( paste("ParentObjOf", zeh.objects.names[combCount], sep = '') , 
                                       paste("image", zeh.objects.names[combCount], "Ind", sep ='')), all.x = TRUE, all.y= FALSE)
        } else {
          myDFmain <- merge( myDFmain, ChildDF[[ind]], by.x = c("parentObjectNumber", "imageParentObjInd"),
                             by.y = c( paste("ParentObjOf", zeh.objects.names[combCount], sep = ''), 
                                       paste("image", zeh.objects.names[combCount], "Ind", sep ='') ), all.x = TRUE, all.y= FALSE)
        }
        
      }
      return(myDFmain)
    } # end combineObjectsFun function
    
    myDFmain<- combineObjectsFun()
    
    # end if statement parentObject is trackedObject  
  } else if ( childObject1 == trackedObject & nchar(trackedObject) > 0){  # each child only 1 parent so no operations needed on childObject1, but childObject2 etc will have to be summarized for each parent that is asigned to the tracked object. 
    
    if( exists("myDFmain"))
    {
      stop("error223")
    }
    
    # only parents that have a tracked object child are taken along ( tracked object defines data ) 
    
    ind <- which(zeh.objects.names=="ChildObject1") 
    myDFmain <- merge(  myDFrawParent, ChildDF[[ind]],  by.x = c("parentObjectNumber", "imageParentObjInd"),
                        by.y = c( "ParentObjOfChildObject1", "imageChildObject1Ind"  ), all.x = FALSE, all.y= TRUE)
    
    deh.objects <- zeh.objects[ !zeh.objects %in% childObject1]
    deh.objects.names <- zeh.objects.names[! zeh.objects.names %in% "ChildObject1" ]
    
    
    combineObjectsChTrFun <- function() {
      
      for(combCount in seq_along(deh.objects)){
        
        ind <- which(zeh.objects.names==deh.objects.names[combCount]) 
        
        ChildDF[[ind]] <- ChildDF[[ind]][ ChildDF[[ind]][paste("ParentObjOf", deh.objects.names[combCount], sep = '')] != 0, ]
        maxSecChildrenPath  <- paste( "Measurements", setDateID, parentObject, paste( "Children", deh.objects[combCount], "Count", sep = "_" ), sep ="/" ) 
        suppressWarnings(SecChildren <-  as.integer(h5read( hdf5FileName, maxSecChildrenPath )[[ "data" ]] ))
        
        fractionMultiple <- sum(SecChildren > 1) / length(SecChildren)
        rm("SecChildren")  
        
        if ( fractionMultiple > thresholdMaxSecChildren  )
        {
          # now within each image, find which objects have identical parent and calculate summary statistic
          #this is a bi*** on computation time
          groupVars <- c( paste("image", deh.objects.names[combCount], "Ind", sep =''), paste("ParentObjOf", deh.objects.names[combCount], sep = ''))
          secVars <- colnames(ChildDF[[ind]])[ !(colnames(ChildDF[[ind]]) %in% groupVars) ]
          if (length(colnames(ChildDF[[ind]])) -2 != length(secVars)) {
            stop("likely the groupVars for secondary were not found")
          }
          nRows <- nrow(ChildDF[[ind]])
          ChildDF[[ind]] <- aggregate(x= ChildDF[[ind]][, secVars],by= ChildDF[[ind]][, groupVars ],  multiplePerParentFunction  ) # can take long 
          colInd <- which(groupVars %in% colnames(ChildDF[[ind]]))
          colnames(ChildDF[[ind]])[-colInd] <- secVars 
          if ( nRows <= nrow(ChildDF[[ind]])){
            stop( "not multiple secondary per parent or aggregration problem")
          }
        }
        if(!exists("myDFmain")){
          
          myDFmain <- merge( myDFrawParent, ChildDF[[ind]], by.x = c("parentObjectNumber", "imageParentObjInd"),
                             by.y = c( paste("ParentObjOf", deh.objects.names[combCount], sep = '') , 
                                       paste("image", deh.objects.names[combCount], "Ind", sep ='')), all.x = TRUE, all.y= FALSE)
        } else {
          myDFmain <- merge( myDFmain, ChildDF[[ind]], by.x = c("parentObjectNumber", "imageParentObjInd"),
                             by.y = c( paste("ParentObjOf", deh.objects.names[combCount], sep = ''), 
                                       paste("image", deh.objects.names[combCount], "Ind", sep ='') ), all.x = TRUE, all.y= FALSE)
        }
        
      }
      return(myDFmain)
    } # end combineObjectsFun function
    
    myDFmain<- combineObjectsChTrFun()     
    
    
  } else {
    stop("Aligning of objects to tracked object failed")
  }
  

  
  #now combine image data.frame with main-dataframe: myDFmain
  
  myDF <-  merge( myDFmain ,myDFImage, by.x = "imageParentObjInd", by.y=  "imageNumber", sort = FALSE ) 
  
  outputList  <- list(myDF = myDF, ktest = "hi")
  return( outputList )
}