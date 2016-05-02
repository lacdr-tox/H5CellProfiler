verifyInputFun <- function(trackedObject=trackedObject, trackingLabel=trackingLabel, trackingParent=trackingParent, setDateID=setDateID,
                           my.objects=my.objects, all.objects=all.objects,
                           is.character0=is.character0,hdf5FileName=hdf5FileName, oscillation=oscillation,
                           locationID=locationID, timeID=timeID, imageID=imageID, replID=replID, plateID=plateID,
                           plateMDFileName=plateMDFileName,hdf5FileNameL=hdf5FileNameL) {
  
  
  
  
  # only parent or childObject1 can be tracked object
#   if(!is.character0(trackedObject))
#     {
#     if ( childObject2 == trackedObject | childObject3 == trackedObject | childObject4 == trackedObject | childObject5 == trackedObject & nchar(trackedObject) > 0 )
#       {
#         stop("only parentObject or childObject1 can be tracked object")
#       }
#     }
  
  
#   if(!is.character(trackedObject))
#   {
#     if ( trackedObject != str_extract(trackingLabel, trackedObject) |
#          trackedObject !=  str_extract(trackingParent, trackedObject) 
#        )
#       {
#         stop( "Faulty defined tracking object / label or parent")
#       }
#   }
  
  # check if all objects are defined that the user wants measured
  
  if (nchar( paste("Measurements", setDateID, sep ="/")) != 32)
  {
    stop("Check setDateID")
  }
  
  if ( nchar(locationID)  < 10 )
  {
    stop("Please define locationID")
  }
  
  
    
    if (!all(  all.objects %in% c(my.objects, "Image"))) {
      stop("Object in myFeaturePathsA found that is not defined in object definitions")
      }
  
  # check if the plate layout template has the same date identifier as the .h5 file 
  xlsInWD <- grepl( plateMDFileName, dir( ) )
  hdf5InWd <- grepl( hdf5FileName, dir( ) )
  if (  all( xlsInWD == FALSE ) | all( hdf5InWd  == FALSE ) ) 
  {
    #stop( "hdf5 or plate layout xls file not in current working directory." )
  }
  # check if the dates of .h5 and xls plate layout match
  dateXls <- str_match( plateMDFileName, "[0-9]{4}[:blank: \\_ \\-]{1}[0-9]{2}[:blank: \\_ \\-]{1}[0-9]{2}" )
  dateH5 <- str_match( hdf5FileName, "[0-9]{4}[:blank: \\_ \\-]{1}[0-9]{2}[:blank: \\_ \\-]{1}[0-9]{2}" )
  if( any(is.na(c(dateXls, dateH5)))) {
    
  } else {
    if ( any( is.na( c( dateXls, dateH5 ) ) ) == TRUE )
    {
      warning( " Unable to find date in plate layout or h5 file names ")
    }
    if ( any( c( substr( dateXls, 1, 4  ) != substr( dateH5, 1, 4 ), substr( dateXls, 6, 7  ) != substr( dateH5, 6, 7 ),
               substr( dateXls, 9, 10  ) != substr( dateH5, 9, 10 ) ) )  )
    {
      warning( "Unable to find match in dates between .h5 and .xls file")
    }
  }
  if (nchar( setDateID ) != 19 )
  {
    stop( "Could not find current h5 data-time ID")
  }
  
    
  # check for dimenstions index tables
  dimStrings <- h5ls(hdf5FileName)$dim
  ind <- grep("x", dimStrings)
  dimStrings <- dimStrings[ind]
  notLength3 <- !grepl("3 x ", dimStrings)
  if( sum(notLength3) != 0 ){
    stop("Hdf5 index table with more than 3 columns found: not implemented.")
  }

  


}