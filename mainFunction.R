source("verifyInputFun.R")
  
#12 nov 2014 23:00
mainFunction <- function(  hdf5FileNameL=hdf5FileNameL,locationID=locationID, timeID=timeID, plateID=plateID,
                           imageID=imageID, replID=replID,myFeaturePathsA=myFeaturePathsA, plateMDFileName=plateMDFileName,
                           parentObject=parentObject, childObject1=childObject1, childObject2=childObject2, 
                           childObject3=childObject3, childObject4=childObject4, childObject5=childObject5, 
                           tertiaryObject=tertiaryObject, multiplePerParentFunction=multiplePerParentFunction,
                           oscillation=oscillation, 
                           writeSingleCellDataPerWell=writeSingleCellDataPerWell, 
                           writeAllSingleCellData=writeAllSingleCellData, h5loop=h5loop,
                           timeBetweenFrames = timeBetweenFrames, exposureDelay = exposureDelay) {

  
  #h5loop=1
  hdf5FileName <- hdf5FileNameL[h5loop]
  # metadata can be manualy defined by user per h5 file, or a single or per h5 file a h5 path
  h5L <- length(hdf5FileNameL)
  if(h5L > 1 )
    if(length(timeID)==1)
    {
      timeID <- rep(timeID, h5L)
    }
    if(length(replID)==1)
    {
      replID <- rep(replID, h5L)
    }
    if(length(plateID)==1)
    {
      plateID <- rep(plateID, h5L)
    }
    if(length(locationID)==1)
    {
      locationID <- rep(locationID, h5L)
    }
    if(length(imageID)==1)
    {
      imageID <- rep(imageID, h5L)
    }
  if(length(timeBetweenFrames)==1)
  {
    timeBetweenFrames <- rep(timeBetweenFrames, h5L)
  }
  if(length(exposureDelay)==1)
  {
    exposureDelay <- rep(exposureDelay, h5L)
  }
  
    
  
  timeID <- timeID[h5loop]  
  replID <- replID[h5loop]  
  plateID <- plateID[h5loop]
  locationID <- locationID[h5loop]
  imageID <- imageID[h5loop]
  timeBetweenFrames <- timeBetweenFrames[h5loop]
  exposureDelay <- exposureDelay[h5loop]
  
  
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
  # "look behind" ?<=
  rmIndIm<- grepl("Image", all.paths.tr)
  all.paths.tr<-all.paths.tr[!rmIndIm]
  trackedObject <- unique(str_extract(all.paths.tr,
                                      '(?<=Measurements/[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}/)[A-Z a-z _ 0-9]+(?=/)'))
  
myFeaturePathsA <- paste( "Measurements", setDateID ,myFeaturePathsA, sep = "/")
  if( !is.character0(trackedObject) ){
    # if LAP tracking is used, track_dist will be NA
    track_dist<-unique(str_extract(all.paths.tr, '[0-9]{1,3}$')  )
    if(length(track_dist) != 1){
      stop("reg expr for track_dist failed")
    }
    if(is.na(track_dist)) {
      track_dist <- NULL
    } else {
      track_dist <- paste0( "_" ,track_dist)
    }


  trackedObjectDisplacement<- paste("TrackObjects_DistanceTraveled", track_dist, sep ="") 
  trackingLabel <- paste(trackedObject, "/", "TrackObjects_Label", track_dist , sep  ="") 

     # dont confuse this with Parent object:
  trackingParent <- paste(trackedObject, "/", "TrackObjects_ParentObjectNumber", track_dist, sep ="") 
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
    if ( grepl("/",timeID )) # timeID can be h5 path or manualy defined per h5 file by user
    {
      timeID <- paste( "Measurements", setDateID, timeID, sep = "/" )
    }
    if ( grepl("/",plateID )) # timeID can be h5 path or manualy defined per h5 file by user
    {
      plateID <- paste( "Measurements", setDateID, plateID, sep = "/" )
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

verifyInputFun(trackedObject=trackedObject,trackingLabel=trackingLabel, 
               trackingParent=trackingParent, setDateID=setDateID,
               my.objects=my.objects, all.objects=all.objects,
               is.character0=is.character0,
               hdf5FileName=hdf5FileName, oscillation=oscillation,
               locationID=locationID, timeID=timeID, imageID=imageID, replID=replID, plateID=plateID,
               plateMDFileName=plateMDFileName, hdf5FileNameL=hdf5FileNameL)

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
    
    # supresswarnings to supress the "h5read data loss 64-bit-32 conversion")
    suppressWarnings(hdf5List <- h5read(  file=hdf5FileName,  name=hdf5Path, bit64conversion='double' )) 
    
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
    #wellicht hier rekening houden met NA waarden nog>>??
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
    stackedIndex$rowInd[100:140]
    hdf5List[["data"]][100:130]
    stackedIndex$imageInd[100:120]
    hdf5List[["data"]][stackedIndex$rowInd][100:120]
    outDF <- data.table(imageNumber = stackedIndex$imageInd ,
                        dataOut     = hdf5List[["data"]][stackedIndex$rowInd],
                        rowInd = stackedIndex$rowInd,
                        key ="imageNumber"
                        
                        )
    setkey(outDF,"imageNumber")
    
    
    setnames(outDF,"dataOut",dataName)
    setnames(outDF,"rowInd", paste(rowIndName, "_rowInd", sep = ""))
    
    return(outDF)
  } # imageInd" are keys ( $sorted )

# the answer is"as.numeric(levels(f))[f]"
# as.numeric(levels(test$imageNumber))[test$imageNumber]
# test$imageNumber



if(!exists("track_dist")){
track_dist <- character(0)
}
# variable columns names:
trackingParentCN = paste(trackedObject, '_TrackObjects_ParentObjectNumber', track_dist, sep = "") # track-parent (timewise)
trackingObjectNumberCN = paste(trackedObject, 'Number_Object_Number', sep = "_")
trackingxCoordCN = paste(trackedObject, 'Location_Center_X', sep = "_")
trackingyCoordCN = paste(trackedObject, 'Location_Center_Y',  sep = "_")
trackingxCoordCN_tMin1 = paste(trackingxCoordCN, 'tMin1', sep = '_')
trackingyCoordCN_tMin1 = paste(trackingyCoordCN, 'tMin1', sep = '_')
trackingLabelCN =  paste(trackedObject, '_TrackObjects_Label', track_dist, sep = '')
parentObjectNumberCN = paste(parentObject, 'Number_Object_Number', sep = "_")
trackingDistanceTraveledCN = paste(trackedObject, '_TrackObjects_DistanceTraveled', track_dist, sep = "")

ImageCountParentsCN <- "imageCountParentObj"

kColNames = 
  list(
    trackingParentCN = trackingParentCN, # track-parent (timewise)
    trackingObjectNumberCN = trackingObjectNumberCN,
    trackingxCoordCN = trackingxCoordCN,
    trackingyCoordCN = trackingyCoordCN,
    trackingxCoordCN_tMin1 = trackingxCoordCN_tMin1,
    trackingyCoordCN_tMin1 = trackingyCoordCN_tMin1,
    trackingLabelCN =  trackingLabelCN,
    parentObjectNumberCN = parentObjectNumberCN,
    trackingDistanceTraveledCN = trackingDistanceTraveledCN,
    ImageCountParentsCN = ImageCountParentsCN
  )

    # all fixed image data needed ( includes group and group index data because CellProfiler should have each location defined as a group - even for single time points:
    # 
  if(!is.character0(trackedObject)) 
  {
    imageCountTrackedPath <- paste( "Measurements", setDateID, "Image", 
                                    paste( "Count", trackedObject, sep ="_" ), sep = "/" )
    dataName <- "imageCountTracked"
    hdf5Path <- imageCountTrackedPath
    rowIndName <- "image"
    imageCountTracked <- hdf5IndexFun( hdf5Path = hdf5Path , dataName = dataName, rowIndName = rowIndName )
  }   else  {
    imageCountParentObjPath <- paste( "Measurements", setDateID, "Image", 
                                    paste( "Count", parentObject, sep ="_" ), sep = "/" )
    dataName <- "imageCountParentObj"
    hdf5Path <- imageCountParentObjPath
    rowIndName <- "image"
    imageCountParentObj <- hdf5IndexFun( hdf5Path = hdf5Path , dataName = dataName, rowIndName = rowIndName )
  }
  
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
if(!is.character0(trackedObject)) 
  {
    myDFrawImage <- myDFrawImage1[imageCountTracked]
    rm('imageCountTracked')
  } else {
    myDFrawImage <- myDFrawImage1[imageCountParentObj]
    rm('imageCountParentObj')
  }

rm( 'myDFrawImage1', 'groupInd', 'groupNumber' )

# add metadata. Once for all the hdf5-path based and once for all manually defined metadata
metaDataList <- c(locationID, imageID, timeID, plateID, replID)
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
currImageData[, timeID:=as.numeric(timeID)]
#currImageData[, imageID:=as.integer(imageID)]
# merge to myDFrawImage
myDFrawImage <-myDFrawImage[currImageData]

# add metadata from csv file to imageDF

metaCSVData <- read.table( plateMDFileName, sep = "\t", header = TRUE, comment.char = "")
metaCSVData<- as.data.table(metaCSVData)
metaCSVData[, plateID:= as.character(plateID)]
setkey(metaCSVData, 'treatment')

# only use data that is defined in metadata layout file, in treatment column
metaCSVData <- metaCSVData[ !"" ]
metaCSVData <- metaCSVData[ !"0" ]
metaCSVData <- metaCSVData[ !0 ]
metaCSVData <- metaCSVData[ !is.na(locationID),]

# make sure that locationId is a character to avoid failing match later on
metaCSVData <- metaCSVData[, locationID:=as.character(locationID)]

if ( length(metaCSVData$treatment) != length(metaCSVData$dose_uM[metaCSVData$dose_uM!="" & !is.na(metaCSVData$dose_uM)]) ){
  stop("Some treatments did not have valid concentration in metadata file")
}

if( !all(unique(myDFrawImage$plateID) %in% unique(metaCSVData$plateID)) 
      )
{
  stop("Could not match all manualy defined ID's to plate layout ID's, please double check the plateID definitions in your layout-file")
}

if(  !all(unique(metaCSVData$plateID) %in% unique(myDFrawImage$plateID)))
{
  warning("Could not match all plate-layout plateID's to plateID's in data: consult \"HDF5notInLayout.txt\" ")
}


myDFrawImage[ , plateWellID:= paste(plateID, locationID, sep ="_")]



#HDF5notInLayout <- !gsub( "(_[0-9]{1,2})$", "", unique(myDFrawImage$plateWellID)) %in% 
#  unique(paste(metaCSVData$plateID, metaCSVData$locationID, sep ="_"))




uniqueWells <- unique(myDFrawImage$plateWellID)

#write.table(file = "HDF5notInLayout.txt", 
#            gsub( "(_[0-9]{1,2})$", "", 
#                  unique(myDFrawImage$plateWellID ))[HDF5notInLayout], sep = "\t"  )


 ind <- match(  gsub( "(_[0-9]{1,2})$", "", myDFrawImage$plateWellID),
                paste(metaCSVData$plateID, metaCSVData$locationID, sep ="_"))
   if ( length( ind ) != sum(!is.na(ind) ) | length( ind ) == 0 ) 
       {
       warning( "CP analysed wells not found, could result in errors consult written text file:  \"HDF5notInLayout.txt\"") # after metadata merge to imagedata
       }
myDFrawImage$joiner <- gsub( "(_[0-9]{1,2})$", "", myDFrawImage[ , locationID ])
setkeyv(myDFrawImage, c("plateID","joiner" ))
setkeyv(metaCSVData, c("plateID","locationID"))
#$ check uniqueness keys
checkUnique <- paste(metaCSVData$locationID, metaCSVData$plateID)
if(nrow(metaCSVData) != length(unique(checkUnique))){
  print(count(paste(metaCSVData$locationID, metaCSVData$plateID))[count(paste(metaCSVData$locationID, metaCSVData$plateID))[, "freq"] >1,])
  stop("duplicate locationID - plateID keys, check layout file")
}

myDFImage <- metaCSVData[myDFrawImage] # make sure CSVData contains all the locations and plateIDs found in hdf5 else NA values in data from missing plate layout values

if(!identical(myDFImage$locationID, gsub("(_[0-9]{1,2})$", "",myDFImage$i.locationID))){
  write.table(file="testingMCmergeMDIMagedataFail.txt", myDFImage[, list(locationID,i.locationID) ], sep ="\t", col.names = T)
  warning("merge metadata and image data failed")
}

# als CP analyzed data niet in layout file zit zitten er NA waarden in myDFImage, dit moet een keer beter

myDFImage$locationID <- NULL
setnames(myDFImage, "i.locationID", "locationID")
# select correct metadata columns (from hdf5, manually or from metadata layout file)
# if NA in metadata layout then these are removed. Else alternative is removed and metadata ones renamed



if(any(is.na(metaCSVData[ , timeID]))) { # if NA in layout file then use the i.timeID (check which one this is)
  myDFImage$timeID <- NULL
   setnames(myDFImage, 'i.timeID', 'timeID')
   
} else {
  myDFImage$i.timeID <- NULL
}



if(any(is.na(metaCSVData[ , replID])) ) {
  
  myDFImage[,replID:=NULL]

    if( "i.replID" %in% colnames(myDFImage) ){
    setnames(myDFImage, 'i.replID', 'replID')
  }
  
} 

# following plate and well/locationID from analyzed images was not probably not found in layout file:
# this is removed before further processing, however user should be notified to verify analysis
missingInLayout <- myDFImage[ is.na(treatment) & is.na(dose_uM) & is.na(control) & is.na(cell_line)  ]
write.table( missingInLayout, file = "HDF5notInLayout.txt", sep = "\t", col.names = TRUE, row.names = FALSE)
myDFImage <- myDFImage[ !(is.na(treatment) & is.na(dose_uM) & is.na(control) & is.na(cell_line) )  ]
# To pass an expression into your own function, one idiom is as follows :
#   > DT = as.data.table(iris)
# > setkey(DT,Species)
# > myfunction = function(dt, expr) {
#   + e = substitute(expr)
#   + dt[,eval(e),by=Species]
#   + }
# > myfunction(DT,sum(Sepal.Width))

#if(nrow(myDFImage) != nrow(myDFrawImage)){
#  warning("merging metadata error: some CP-analysed data not found in plate-lyaout file, consult written text file" )
#  }

if(any(is.na(myDFImage$imageNumber))){
  stop("missing data in metadata file")
}
rm("myDFrawImage")  

# save imagenumber and location metadata for veryficiation internal ordering:
write.table(myDFImage[, list(imageNumber, groupNumber, groupInd, locationID, imageID )],
            file = "veryfy internal ordering.txt", sep = "\t", row.names = FALSE)



  #=====
  #remove two levels from "feature"- path, use this for other paths in h5 file
  #exprPathI <- gsub( "([/][^/]*)$", "", firstFeature  )
  #=====
  
  #now add all info to the corresponding dataframes using regular expressions of the ( if exists ) 4 object definitions + the image path
# pleur alles in myFeaturePaths, combineer alles per object, voeg indexen toe:

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
myFeaturePaths<- unique(myFeaturePaths)
myFeaturesData = list()   
if ( ( length(myFeaturePaths) != 0) ) {
  for ( i in 1 : length( myFeaturePaths ) ) 
    {
    
      hdf5Path <- myFeaturePaths[[i]]
      preName <- gsub(paste("Measurements/", setDateID, sep =""), "", myFeaturePaths[[i]])
      pat<-"([/][[:alnum:][:digit:][:punct:]]*[/])" 
    rowIndName <- gsub("/","",str_match(preName, pat ))[1]
    dataName <- gsub( paste( "/", rowIndName, sep =""), "", preName) 
    dataName <- gsub("[/]", "", dataName)
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
myDTParentList <- lapply(myDTParentList, function(x) x<- setkeyv(x, c(objName, "imageNumber")))

myDTParent <- myDTParentList[[1]]

if(length(myDTParentList) > 1 ){
  
  for (cbloop in 1: (length(myDTParentList)-1) ) {
    myDTParent <- myDTParent[myDTParentList[[cbloop+1]]]
  }
}
rm("myDTParentList")

ind.im <- grep('i.imageNumber[0-9]{0,2}',colnames(myDTParent))
  if(!is.integer0(ind.im))
    {
    myDTParent[,c(ind.im) := NULL ]
    }
ind.im <- grep('(_rowInd)$',colnames(myDTParent))
if(!is.integer0(ind.im))
{
  myDTParent[,c(ind.im) := NULL ]
}
setnames(myDTParent, colnames(myDTParent), 
         paste(parentObject, colnames(myDTParent) , sep ="_"))

#change factors to numeric:
# indFac <- unlist(lapply(myDTParent, is.factor))
# namesFac<-names(myDTParent)[indFac]
# for (col in namesFac) 
# {#as.numeric(levels(f))[f]
#   set(myDTParent, j=col, value=as.integer(myDTParent[[col]]))
# }

# parent DT has been created, loop through remaining sec./tert. objects and merge to myDTParent
# child 1
#loop through all objects in myFeaturesData, except parentObj (alrdy done) and except image object (needs to be handled differently)
child.objects <- unique(names(myFeaturesData))
child.objects<- child.objects[!child.objects %in% c(parentObject, "Image")]
print(paste("Looping through child objects: ", paste(child.objects, collapse = " & ")))
for (childLoop in seq_along(child.objects))
{
  currChildObj <- child.objects[childLoop]
  currChildInd <- currChildObj == names(myFeaturesData)
  currChildList <- myFeaturesData[currChildInd]
  objName <- child.objects[childLoop]
  objName <- paste(objName, 'rowInd', sep = '_')
  currChildList <- lapply(currChildList, function(x) x<- setkeyv(x, c(objName, "imageNumber")))
    myDTCurrChild <- currChildList[[1]]
      if(length(currChildList) > 1 )
      {
        for (cbloop in 1: (length(currChildList)-1) ) 
        {
          myDTCurrChild <- myDTCurrChild[currChildList[[cbloop+1]]]
        }
      }
      ind.im <- grep('imageNumber.[0-9]{1,2}',colnames(myDTCurrChild))
      if(!is.integer0(ind.im))
      {
        myDTCurrChild[,c(ind.im) := NULL ]
      }
  ind.im <- grep("_rowInd$",colnames(myDTCurrChild))
  if(!is.integer0(ind.im))
  {
    myDTCurrChild[,c(ind.im) := NULL ]
  }
    
  setnames(myDTCurrChild, colnames(myDTCurrChild), 
           paste(currChildObj, colnames(myDTCurrChild) , sep ="_"))
  
  #change factors to integer
#   indFac <- unlist(lapply(myDTCurrChild, is.factor))
#   namesFac<-names(myDTCurrChild)[indFac]
#   for (col in namesFac) 
#     {
#     set(myDTCurrChild, j=col, value=as.integer(myDTCurrChild[[col]]))
#     }
  
  
    # calculate summary per parent object & merge to myDTParent:
  setkeyv(myDTCurrChild, c(paste(currChildObj, "imageNumber", sep = "_"), 
                           paste(currChildObj, "Parent", parentObject, sep ="_")))
  
  
  myDTCurrChild<-myDTCurrChild[, lapply(.SD, multiplePerParentFunction), 
                  by =  c(paste(currChildObj, "imageNumber", sep = "_"), 
                          paste(currChildObj, "Parent", parentObject, sep ="_")) ]
  setkeyv(myDTCurrChild, c(paste(currChildObj, "imageNumber", sep = "_"),
                           paste(currChildObj, "Parent", parentObject, sep ="_")))
  setkeyv(myDTParent, c(paste(parentObject, "imageNumber", sep ="_"), 
                        c(paste(parentObject, "Number_Object_Number", sep ="_"))))
  
  
  #X[Y] looks up rows in X using key of Y:
  # Keep the Parent DF as is, and change the names back to original parent names
  myDTParent <- myDTCurrChild[myDTParent]
  setnames(myDTParent, 
           c(paste(currChildObj, "imageNumber", sep = "_"),
                         paste(currChildObj, "Parent", parentObject, sep ="_")),
           c(paste(parentObject, "imageNumber", sep ="_"), 
             c(paste(parentObject, "Number_Object_Number", sep ="_"))))
}
if(exists('currChildList')){
rm('currChildList')}

# did not test user defined image addition yet
# image object myFeatures data (the image object measurements defined by user..)
imageInd <- "Image" == names(myFeaturesData)
if(any(imageInd))
  {
    myDTImageList <- myFeaturesData[imageInd]
    objName <- unique(names(myDTImageList))
    objName <- paste(objName, 'rowInd', sep = '_')
    myDTImageList <- lapply(myDTImageList, function(x) x<- setkeyv(x, objName))
    myDTImage <- myDTImageList[[1]]

    if(length(myDTImageList) > 1 )
      {
  
      for (cbloop in 1: (length(myDTImageList)-1) ) 
        {
        myDTImage <- myDTImage[myDTImageList[[cbloop+1]]]
        }
      }
  rm("myDTImageList")

    ind.im <- grep('imageNumber.[0-9]{1,2}',colnames(myDTImage))
    if(!is.integer0(ind.im))
      {
        myDTImage[,c(ind.im) := NULL ]
      }
    ind.im <- grep('(_rowInd)$',colnames(myDTImage))
      if(!is.integer0(ind.im))
        {
        myDTImage[,c(ind.im) := NULL ]
        }
    setnames(myDTImage, colnames(myDTImage), 
         paste("Image", colnames(myDTImage) , sep ="_")) 

  setkey(myDFImage, "imageNumber")
  setkeyv(myDTImage, paste("Image", "imageNumber", sep ="_"))

myDFImage <- myDTImage[myDFImage]
setnames(myDFImage, paste("Image", "imageNumber", sep ="_"), "imageNumber")
}

rm('myFeaturesData')

# combine image data.frame with main-dataframe: myDFmain

indrowInd <- grep('image_rowInd[\\.[:alnum:]]{0,3}', colnames(myDFImage))
myDFImage[, c(indrowInd) := NULL]
#hierhier


imName<- paste(parentObject,"imageNumber" , sep = "_")
myDTParent[, eval(imName):=as.integer(levels(get(imName)))[get(imName)]]
myDFImage[, imageNumber:=as.integer(levels(imageNumber))[imageNumber]]





setkeyv(myDTParent, imName)  
setkey(myDFImage, imageNumber)
#check for duplicate keys: allow.cartesian is now ignored when i has no duplicates
if(length(myDFImage$imageNumber) != length(unique(myDFImage$imageNumber))){
  stop("duplicate imageNumber keys in myDFImage")
}
myDT<-myDFImage[myDTParent] # soms missing images omdat geen objecten in myDTParent, daarom nu omgedraaid# , allow.cartesian = TRUEis dit verstandig? 



#myDT <- myDFImage[myDTParent]


#X[Y] looks up rows in X using key of Y:
#myDT <- myDTParent[myDFImage]
ind.rm<-grep( "(i\\.imageNumber)$", colnames(myDT))
if(length(ind.rm)!=0) {
myDT[,c(ind.rm):=NULL]
}

rm("myDTParent", "myDFImage")  

timeBetweenFrames <- round(as.integer(strftime(strptime(timeBetweenFrames, format = "%H:%M:%S"), "%H")) + 
                             1/60 * as.integer(strftime(strptime(timeBetweenFrames, 
                                                                 format = "%H:%M:%S"), "%M")) +
                             1/3600 * as.integer(strftime(strptime(timeBetweenFrames, 
                                                                 format = "%H:%M:%S"), "%S"))
                             , digit =2 )

exposureDelay <- round(as.integer(strftime(strptime(exposureDelay, format = "%H:%M"), "%H")) + 
                         1/60 * as.integer(strftime(strptime(exposureDelay, 
                                                             format = "%H:%M"), "%M")), digit =1 )
myDT[, timeAfterExposure:=timeID*timeBetweenFrames+exposureDelay - timeBetweenFrames ]

if("replID"%in% colnames(myDT)){
myDT[, replID:=as.factor(replID)]
}
myDT[, timeID:=as.factor(timeID)]

#myDT[, dose_uM:=as.numeric(dose_uM)]
myFeature<-gsub(paste(paste("Measurements", setDateID, sep ="/"), "/", sep =""),"", myFeature)
myFeature<-gsub("/","_", myFeature)

# remove rows with NA treatment
myDT <- myDT[ !is.na(treatment)]

sumData <- myDT[, lapply(.SD,  
                          function(x) {mean(as.numeric(x), na.rm = TRUE)}
                          
), 
by =  c("treatment", "timeID", "dose_uM", "plateID"),
.SDcols = eval(myFeature)]



if (writeSingleCellDataPerWell){
  newDir <-paste( gsub(".h5", "",   hdf5FileName), "perLocation", sep = " ")
  if ( !file.exists( newDir )) 
  {
    dir.create( newDir )
  }
  
  setkey(myDT, locationID)
  for ( i in seq_along(uniqueWells) )
  {
    subSetWells <-   myDT[ uniqueWells[i]]
    tmpTreat <- unique(subSetWells$treatment)
    tmpfileName <- paste(tmpTreat, uniqueWells[i], sep = '_')
    write.table( subSetWells, 
      file =  paste( newDir, "/", tmpfileName, ".txt", sep = ""),
                 sep = "\t", row.names = FALSE )
  
  }
}

# write all ordered data
if (writeAllSingleCellData) {
  write.table( myDT, 
               file = paste( gsub(".h5", "",   hdf5FileName), "singleCellData.txt", sep = "_"),
               sep = "\t", row.names=FALSE )
}
if(h5loop == length(hdf5FileNameL)) 
  {
outputList  <- list(myDT = myDT, sumData=sumData, kColNames = kColNames, myFeaturePaths=myFeaturePaths,
                      myFeaturePathsA=myFeaturePathsA , metaCSVData=metaCSVData,
                    hdf5FileNameL=hdf5FileNameL, numberCores=numberCores, metaDataList=metaDataList,
                    plateMDFileName=plateMDFileName)
  } else {
          outputList <- list(myDT = myDT, sumData=sumData)
         }

outputList

}


