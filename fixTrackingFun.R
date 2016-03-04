fixTrackingFun <- function(myDFstukkie, myFeatures, i) {
  
  
  trackingParentCN <- kColNames$trackingParentCN
  trackingObjectNumberCN <- kColNames$trackingObjectNumberCN
  trackingxCoordCN <- kColNames$trackingxCoordCN
  trackingyCoordCN <- kColNames$trackingyCoordCN
  trackingxCoordCN_tMin1 <- kColNames$trackingxCoordCN_tMin1
  trackingyCoordCN_tMin1 <- kColNames$trackingyCoordCN_tMin1
  trackingLabelCN <- kColNames$trackingLabelCN
  parentObjectNumberCN <- kColNames$parentObjectNumberCN
  trackingDistanceTraveledCN <- kColNames$trackingDistanceTraveledCN
  ImageCountParentsCN <- kColNames$ImageCountParentsCN
  
  
  directionality.data.list = list()
  allTrackDF.list = list()
  setkey(myDFstukkie, locationID)
  currentUniqueWells<-uniqueWellGroups[[i]]
  currentUniqueWells<-factor(currentUniqueWells)
  for (counterStukkie in seq_along( currentUniqueWells)) {   
    
    myAlltrack <- myDFstukkie[ list(currentUniqueWells[counterStukkie]) ]
    #select all info of interest:
    
    
    # if ( length(match(selCols, colnames(myDFo))) != length(selCols)){
    #    stop("didnt find all columns for trackOrder data selection")
    #  }
    if(length(unique(myAlltrack$groupNumber)) >1 )
    {
      stop("Defined location (uniqueWells) mutliple group numbers = multiple locations - not allowed")
    }
    t.n <- max(myAlltrack$groupInd)
    
    # remove of identical parent numbers those with the non-shortest distance to respective parent.
    if(findMinIdenticalParents) {
     
      # Per time frame (groupInd) count (.N) the number of cells that have the
      # same tracking parent (trackingParentCN).
      # The get is necessary because trackingParentCN is a column name (CN
      # suffix) as string, but as a side effect the column with
      # trackingParentCN in multipleP is now also named 'get'.
      # multipleP will contain 3 columns: time frame (groupInd), the tracking
      # parent (get), and how many objects have that tracking parent
      # (multipleParents).
      multipleP <- myAlltrack[, list(multipleParents = .N), by = list(groupInd, get(trackingParentCN))]

      # Join the information in myAlltrack (regular tracking data) with the
      # multipleP information. Rows are matched by time frame and tracking
      # parent and the added multipleParent column shows how many objects share
      # that same tracking parent.
      # X[Y] is a join, looking up X's rows using Y (or Y's key if it has one)
      # as an index.  The keys from Y disappear whereas the keys from X are
      # kept.
      setkeyv(myAlltrack, c('groupInd', trackingParentCN))
      setkeyv(multipleP, c('groupInd', 'get'))
      myAlltrack <- myAlltrack[multipleP]
      
      # A 0 tracking parent means no parent (= first observation of object), so
      # objects with tracking parent 0 should not be considered as having
      # identical tracking parents. We fix this by setting the mulipleparents
      # value to 1 when the tracking parent is 0.
      setkeyv(myAlltrack, trackingParentCN)
      myAlltrack[list(0), multipleParents := 1L]
      
      # Print the fraction of objects with duplicate parent objects in this
      # part of the data.
      multFrac <- round(sum(myAlltrack[, multipleParents > 1]) / nrow(myAlltrack), digits = 2)
      print(paste(currentUniqueWells[counterStukkie], ": contains", multFrac, "fraction of cells with same multiple parent numbers"))
      
      # Now we want to match objects over 2 time frames to determine which of
      # the objects with shared parent number is closest to the parent in the
      # next time frame.  We make a duplicate of the data.table with the
      # information we need about the earlier (t-1) time frame. (The with =
      # FALSE is necessary so that the variable names are not interpreted as
      # column names, see ?data.table)
      myAlltracktMin1 <- myAlltrack[, c(trackingParentCN, trackingLabelCN,
                                        trackingxCoordCN, trackingyCoordCN,
                                        trackingObjectNumberCN, 'groupInd' ),
                                    with = FALSE
                                    ]

      # Append '_tMin1' to all the column names.
      setnames(myAlltracktMin1, colnames(myAlltracktMin1),
               paste(colnames(myAlltracktMin1),
                     "tMin1", sep ="_"))

      # Now we increase the time frame (groupInd_tMin1) by 1 so we can match
      # with time frame at point t (groupInd).
      myAlltracktMin1[, groupInd_tMin1 := groupInd_tMin1 + 1]
     
      # Now we join the original data with the t-1 data. We match the time
      # frame from t-1 (groupInd_tMin1, which we increased by 1) with the time
      # frame at time t (groupInd), and most importantly, the tracking object
      # number at t-1 with the tracking parent of t.
      setkeyv(myAlltracktMin1, c("groupInd_tMin1", paste( trackingObjectNumberCN, "tMin1", sep = "_")))
      setkeyv(myAlltrack, c("groupInd", trackingParentCN))
      myAlltrackBoth <- myAlltracktMin1[myAlltrack]
      
      # Now select the objects with multiple parents...
      myAlltrackBothWithMP <- myAlltrackBoth[multipleParents > 1 & !is.na(multipleParents)]
      # ... and calculate the distance to that parent.
      myAlltrackBothWithMP[ , distMP := sqrt((get(trackingxCoordCN_tMin1) - get(trackingxCoordCN))^2 +
                                               (get(trackingyCoordCN_tMin1) - get(trackingyCoordCN))^2)]

      # Because in the join the names with '_tMin1' were kept, we need to reset
      # them to the original names.
      setnames(myAlltrackBothWithMP, c("groupInd_tMin1", paste(trackingObjectNumberCN, 'tMin1', sep = '_')),
               c("groupInd", trackingParentCN))
      
      # Now determine for each tracking parent the index (.I) of the object
      # which is closest...
      Ind_Min <- myAlltrackBothWithMP[ , list(Ind_Min=.I[which.min(distMP)]), by = c("groupInd", trackingParentCN)]
      # ... and convert these indices to a vector.
      Ind_Min <- Ind_Min[, Ind_Min]

      # Now from the data.table with objects that have duplicate parents,
      # select the time frame and object number of those that are not closest
      # to the parent object. We want to disconnect these objects from their
      # parents by setting their tracking parent to 0.
      keepMPs <- myAlltrackBothWithMP[!Ind_Min, c("groupInd", trackingObjectNumberCN), with = FALSE]

      # Now, by doing a join, find these objects in data.table with the
      # original tracking info, and set the tracking parent to 0.
      setkeyv(myAlltrack, c("groupInd", trackingObjectNumberCN))
      myAlltrack[keepMPs, trackingParentCN := 0, with = FALSE]
      
      
    } # end if findIdenticalParents
    # Now there are only unique track parent numbers , select and merge time points in wide format using a loop over the time points. However - no objects may go lost so merge with all = TRUE
    min.t <- min(myAlltrack$groupInd)
    max.t <- max(myAlltrack$groupInd)
    
    
    # up till here data.table was used. Not realy huge speed increase because small data pieces in loops and too large time investment to re-write everything. Will only re write pieces with very large tables from here on
    myAlltrack <- as.data.frame(myAlltrack)
    
    
    trackData <- myAlltrack[ myAlltrack$groupInd == max.t, ]
    colnames(trackData) <- paste(colnames(trackData), "TP", max.t, sep ="_")  
    
    nnColnames <-  c("groupNumber", 
                     trackingDistanceTraveledCN, # old displacement
                     
                     "locationID",       
                     "treatment",
                     "dose_uM",
                     "plateID",
                     "replID",
                     "multipleParents",
                     "cell_line",
                     "control"
    )
    
    
    for (timeInd in max.t: (min.t+1)){
      
      toPasteWide <- myAlltrack[ myAlltrack$groupInd == (timeInd-1), ]
      toPasteWide <-toPasteWide[ , !colnames(toPasteWide) %in% nnColnames]
      
      colnames(toPasteWide) <- paste(colnames(toPasteWide), "TP", (timeInd - 1), sep ="_")  
      
      trackData <- merge(trackData, toPasteWide, 
                         by.x = paste(trackingParentCN, "TP", (timeInd ), sep ="_") ,
                         by.y =  paste(trackingObjectNumberCN, "TP", (timeInd - 1), sep ="_") , all = TRUE, sort= FALSE)
      
      
    }
    
    
    if(!file.exists("trackOrderedData")){
      dir.create("trackOrderedData")
    }
    if(!file.exists("trackOrderedData/uniqueParentRawTrackData")){
      dir.create("trackOrderedData/uniqueParentRawTrackData")
    }
    
    
    # write data (unique parents but no reconnecting of tracks yet)
    if( writeUniqueParentsNoRec){
      write.table(trackData, file = paste("trackOrderedData/uniqueParentRawTrackData/", currentUniqueWells[ counterStukkie],"_uniqueParentRawTrackData.txt", sep =""), sep = "\t", col.names = NA)
    }
    
    
    # voor de huidige locatie/ film
    #1 maak x en y-coord tabellen
    #2 maak displacement tabellen
    #3 gebruik oude code beneden voor tracks connecten
    #4 schrijf per locatie de data met tijd als columns en rijen als tracks
    #4 geef tracks een niewe label, combineer alle data in 1 long format tabel
    #5 plot gebruik makend van deze tabel.
    
    
    # test if operations needed can be performed on multiple data.frames simultaneously
    
    
    
    # for testing purposes, save original to compare to reconnected tracks
    
    # reformat the data: single variable with time dataframes in a list
    if(!file.exists("trackOrderedData/beforeCombineTracks")){
      dir.create("trackOrderedData/beforeCombineTracks")
    }
    
    
    # select features, and store them in seperate data.frames, store each dataframe in entries of list
    singleFeatList = list()
    myFeatures <- myFeatures[myFeatures!=trackingDistanceTraveledCN]
    for(greppenCount in seq_along(myFeatures))
    {
      
      ind <- rev(grep(paste("^", myFeatures[greppenCount], "_TP_[0-9]{1,3}$" , sep ='') , colnames(trackData) ))
      
      if(length(ind) == 0L & is.integer(ind)){
        stop("grep fail")
      }
      if(length(ind)!= (max.t - min.t + 1)){ # fix for problem with grepping also the division columns of the selected myFeature (identical strings pieces)
        
        test.colnames <- colnames(trackData)[ind]
        test.colnames <- gsub("TP_[0-9]{1,5}", "", test.colnames)
        
        charlengths<-lapply(test.colnames, nchar)
        if(length(unique(charlengths))!=2){
          stop("danger zone, yes this is an Archer quote")
        }
        maxString <- max(unlist(charlengths))
        ind<- ind[charlengths < maxString]
      }
      test.colnames <- colnames(trackData)[ind]
      if( length(unique(gsub("TP_[0-9]{1,5}", "", test.colnames))) !=1) {
        stop("grepping division column failed")
      }
      
      
      singleFeatList[[greppenCount]] <- trackData[,ind]
      colnames(singleFeatList[[greppenCount]]) <-  str_extract(colnames(singleFeatList[[ greppenCount  ]]), 'TP_[0-9]{1,3}$')
      
      names(singleFeatList)[greppenCount] <- myFeatures[greppenCount]
      
      
      
    } # end greppencount loop
    if( writeBeforeCombineTracks) {
      write.table(singleFeatList[1], file = paste("trackOrderedData/beforeCombineTracks/",
                                                  currentUniqueWells[ counterStukkie],"_beforeTrackConnectFirstFeat.csv", sep ='') , sep=",")
    }
    if( reconnect_tracks ) # this connects tracks that can be directly linked
    {
      # combine tracks
      # strategy: first left sided additions, then right sided additions. First connect tracks that are 1 frame appart
      # step 1) locate within each row the location where an NA value starts looking from t_end to t_start
      # step 2) find all locations in all other rows where there is an NA on t+1 and data on t
      # step 3) calculate distances and determine minimal distance + if it is within limit
      # step 4) reorganize data.
      # step 5) repeat but now with reorganized data
      
      
      # find for each row the locations of the NA that is before non NA's
      NA_left.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) min(which(!is.na(x))) -1) 
      NA_right.ind <- apply(singleFeatList[[trackingyCoordCN]], MARGIN = 1, FUN = function(x) max(which(!is.na(x))) ) 
      # starting at the end, calculated distance matrix and determine which are to be connected based on thresh hold
      for ( m.time in (t.n-1) : 1) {
        
        ind <- which(NA_left.ind == m.time )
        
        x_coord <- singleFeatList[[trackingxCoordCN]][ ind, m.time + 1 ] #current coords of short track  at t +1
        y_coord <- singleFeatList[[trackingyCoordCN]][ ind, m.time + 1 ]  
        
        #now find out where at t is a value and at t + 1 a NA
        
        ind.other <- which(NA_right.ind == m.time)
        
        if( length(ind) !=0 & length(ind.other) != 0)
        {
          
          print(paste("Track-connecting: calculating distance matrix for time point #", m.time ,sep =" "))
          
          
          other_x_coords <- singleFeatList[[trackingxCoordCN]][ ind.other, m.time  ]
          other_y_coords <- singleFeatList[[trackingyCoordCN]][ ind.other, m.time  ]
          
          dist.matrix.x <- outer(x_coord, other_x_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
          dist.matrix.y <- outer(y_coord, other_y_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
          dist.matrix.xy <- sqrt(dist.matrix.x^2 + dist.matrix.y^2)
          # now determine global minima untill no more entries in distance matrix bellow threshhold
          curr.min <- min(dist.matrix.xy[!is.na(dist.matrix.xy)], na.rm=TRUE)
          if(!is.numeric(curr.min)){
            stop("curr.min is not numeric")
          }
          while( curr.min <= max_pixel_reconnect1 ) # within the m.time column, keep finding locations where distance is bellow threshold
          {
            
            min.dist.ind <-  which(curr.min == dist.matrix.xy, arr.ind= TRUE)
            if(length(min.dist.ind) > 2) {# if there are 2 current minima a random one is chosen:
              min.dist.ind <- min.dist.ind[1,]
            }     
            
            
            min.r <- min.dist.ind[1]
            min.c <- min.dist.ind[2]
            ind.r <- ind[min.r]
            ind.r.other <- ind.other[min.c]
            
            
            # use the indexes to perform reshuffling of tracks on all list entries (each list entry is a 2D table of each feature to be plotted/ saved as text file)
            
            for(countFeat in seq_along(singleFeatList))
            {
              
              singleFeatList[[ countFeat ]][ind.r,][!is.na(singleFeatList[[ countFeat ]][ind.r.other,])] <-
                singleFeatList[[ countFeat ]][ind.r.other, ][!is.na(singleFeatList[[ countFeat ]][ind.r.other, ])]
              
              singleFeatList[[ countFeat ]] <- singleFeatList[[ countFeat ]][ -ind.r.other, ]
              rownames(singleFeatList[[ countFeat ]]) <- 1:nrow(singleFeatList[[ countFeat ]])
            }
            
            # recalculate distance matrix using updated data etc      
            NA_left.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) min(which(!is.na(x))) -1) 
            NA_right.ind <- apply(singleFeatList[[trackingyCoordCN]], MARGIN = 1, FUN = function(x) max(which(!is.na(x))) ) 
            
            ind <- which(NA_left.ind == m.time )
            
            x_coord <- singleFeatList[[trackingxCoordCN]][ ind, m.time + 1 ] #current coords of short track  at t +1
            y_coord <- singleFeatList[[trackingyCoordCN]][ ind, m.time + 1 ]  
            
            #now find out where at t is a value and at t + 1 a NA
            
            ind.other <- which(NA_right.ind == m.time)
            if( length(ind) !=0 & length(ind.other) != 0)
            {
              
              
              other_x_coords <- singleFeatList[[trackingxCoordCN]][ ind.other, m.time  ]
              other_y_coords <- singleFeatList[[trackingyCoordCN]][ ind.other, m.time  ]
              
              dist.matrix.x <- outer(x_coord, other_x_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
              dist.matrix.y <- outer(y_coord, other_y_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
              dist.matrix.xy <- sqrt(dist.matrix.x^2 + dist.matrix.y^2)
              
              curr.min <- min(dist.matrix.xy[!is.na(dist.matrix.xy)],na.rm = TRUE)
            } else {# inner if 
              curr.min <-100000 # bigger than max_pixel_reconnect1 so will break loop
            }
            
          } # end while loop
          
        } # if indexes are not null, else check next time point
        
      }  #m.time - loop
      
      if(writeAfterFirstConnect) {
        write.table(singleFeatList[1], file = paste("trackOrderedData/beforeCombineTracks/",
                                                    currentUniqueWells[counterStukkie],"_afterFirstTrackConnectFirstFeat.csv", sep =''), sep=",")
      }
      
    } # if reconnect = 1
    
    
    if ( reconnect_frames > 1 ){
      # now through the second round of reconecting (2 frames appart), using the reconnected data of 1 frame appart
      # find for each row the locations of the NA that is before non NA's
      NA_left.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) min(which(!is.na(x))) -1) 
      NA_right.ind <- apply(singleFeatList[[trackingyCoordCN]], MARGIN = 1, FUN = function(x) max(which(!is.na(x))) ) 
      
      # starting at the end, calculated distance matrix and determine which are to be connected based on thresh hold
      for ( m.time in (t.n-1) : 1) {
        
        ind <- which(NA_left.ind == m.time)
        
        x_coord <- singleFeatList[[trackingxCoordCN]][ ind, m.time + 1 ] #current coords of short track  at t +1
        y_coord <- singleFeatList[[trackingyCoordCN]][ ind, m.time + 1 ]  
        
        #now find out where at t is a value and at t + 1 a NA
        
        ind.other <- which(NA_right.ind == (m.time - 1) )
        if( length(ind) !=0 & length(ind.other) != 0)
        {
          
          print(paste("Track-connecting over 2 frames: calculating distance matrix for time point #", m.time ,sep =" "))
          
          other_x_coords <- singleFeatList[[trackingxCoordCN]][ ind.other, m.time - 1]
          other_y_coords <- singleFeatList[[trackingyCoordCN]][ ind.other, m.time - 1 ]
          
          dist.matrix.x <- outer(x_coord, other_x_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
          dist.matrix.y <- outer(y_coord, other_y_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
          dist.matrix.xy <- sqrt(dist.matrix.x^2 + dist.matrix.y^2)
          # now determine global minima untill no more entries in distance matrix bellow threshhold
          curr.min <- min(dist.matrix.xy[!is.na(dist.matrix.xy)], na.rm=TRUE)
          
          if(!is.numeric(curr.min)){
            stop("curr.min is not numeric")
          }
          while( curr.min <= max_pixel_reconnect2 ) # within the m.time column, keep finding locations where distance is bellow threshold
          {
            
            min.dist.ind <-  which(curr.min == dist.matrix.xy, arr.ind= TRUE) 
            
            if(length(min.dist.ind) > 2) {# if there are 2 current minima a random one is chosen:
              min.dist.ind <- min.dist.ind[1,]
            }
            
            min.r <- min.dist.ind[1]
            min.c <- min.dist.ind[2]
            ind.r <- ind[min.r]
            ind.r.other <- ind.other[min.c]
            
            # use the indexes to perform reshuffling of tracks on all list entries (each list entry is a 2D table of each feature to be plotted/ saved as text file)
            
            for(countFeat in seq_along(singleFeatList))
            {
              singleFeatList[[ countFeat ]][ind.r,][!is.na(singleFeatList[[ countFeat ]][ind.r.other,])] <-
                singleFeatList[[ countFeat ]][ind.r.other, ][!is.na(singleFeatList[[ countFeat ]][ind.r.other, ])]
              #interpolate missing values
              singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]] <- 
                mean(unlist(c(singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]-1],
                              singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]+1])), na.rm = TRUE)
              
              # delete moved old vector
              singleFeatList[[ countFeat ]] <- singleFeatList[[ countFeat ]][ -ind.r.other, ]
              rownames(singleFeatList[[ countFeat ]]) <- 1:nrow(singleFeatList[[ countFeat ]])
            }      
            ###
            
            # recalculate distance matrix using updated data etc      
            NA_left.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) min(which(!is.na(x))) -1) 
            NA_right.ind <- apply(singleFeatList[[trackingyCoordCN]], MARGIN = 1, FUN = function(x) max(which(!is.na(x))) ) 
            
            ind <- which(NA_left.ind == m.time )
            x_coord <- singleFeatList[[trackingxCoordCN]][ ind, m.time + 1 ] #current coords of short track  at t +1
            y_coord <- singleFeatList[[trackingyCoordCN]][ ind, m.time + 1 ]  
            
            #now find out where at t is a value and at t + 1 a NA
            
            ind.other <- which(NA_right.ind == (m.time-1))
            if( length(ind) !=0 & length(ind.other) != 0)
            {
              
              other_x_coords <- singleFeatList$xCoord[ ind.other, m.time - 1 ]
              other_y_coords <- singleFeatList$yCoord[ ind.other, m.time - 1]
              
              dist.matrix.x <- outer(x_coord, other_x_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
              dist.matrix.y <- outer(y_coord, other_y_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
              dist.matrix.xy <- sqrt(dist.matrix.x^2 + dist.matrix.y^2)
              
              curr.min <- min(dist.matrix.xy[!is.na(dist.matrix.xy)],na.rm = TRUE)
            } else {
              curr.min <- 100000 # will break while loop
            }
          } # while curr.min <- max pixel reconn
          
        } # if indexes found
      }  #m.time - loop
      
      if(writeAfterSecondReconnect) {
        write.table(singleFeatList[1], file = paste("trackOrderedData/beforeCombineTracks/",
                                                    currentUniqueWells[counterStukkie],"_afterSecondTrackConnectFirstFeat.csv", sep =''), sep =",")
      }
    } # end if reconnect 2 frames
    
    
    if ( reconnect_frames > 2 ){
      # now through the third round of reconecting (3 frames appart), using the reconnected data of 1 frame appart and 2 frames appart
      # find for each row the locations of the NA that is before non NA's
      NA_left.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) min(which(!is.na(x))) -1) 
      NA_right.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) max(which(!is.na(x))) ) 
      # starting at the end, calculated distance matrix and determine which are to be connected based on thresh hold
      for ( m.time in (t.n-1) : 1) {
        
        ind <- which(NA_left.ind == m.time )
        
        
        x_coord <- singleFeatList[[trackingxCoordCN]][ ind, m.time + 1 ] #current coords of short track  at t +1
        y_coord <- singleFeatList[[trackingyCoordCN]][ ind, m.time + 1 ]  
        
        #now find out where at t is a value and at t + 1 a NA
        
        ind.other <- which(NA_right.ind == (m.time-2))
        if( length(ind) !=0 & length(ind.other) != 0)
        {
          print(paste("Track-connecting over 3 frames: calculating distance matrix for time point #", m.time ,sep =" "))
          
          
          other_x_coords <- singleFeatList[[trackingxCoordCN]][ ind.other, m.time -2  ]
          other_y_coords <- singleFeatList[[trackingyCoordCN]][ ind.other, m.time- 2  ]
          dist.matrix.x <- outer(x_coord, other_x_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
          dist.matrix.y <- outer(y_coord, other_y_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
          dist.matrix.xy <- sqrt(dist.matrix.x^2 + dist.matrix.y^2)
          # now determine global minima untill no more entries in distance matrix bellow threshhold
          curr.min <- min(dist.matrix.xy[!is.na(dist.matrix.xy)], na.rm=TRUE)
          if(!is.numeric(curr.min)){
            stop("curr.min is not numeric")
          }
          while( curr.min <= max_pixel_reconnect3 ) # within the m.time column, keep finding locations where distance is bellow threshold
          {
            
            min.dist.ind <-  which(curr.min == dist.matrix.xy, arr.ind= TRUE)
            
            if(length(min.dist.ind) > 2) {# if there are 2 current minima a random one is chosen:
              min.dist.ind <- min.dist.ind[1,]
            }      
            
            min.r <- min.dist.ind[1]
            min.c <- min.dist.ind[2]
            ind.r <- ind[min.r]
            ind.r.other <- ind.other[min.c]
            
            
            for(countFeat in seq_along(singleFeatList))
            {
              singleFeatList[[ countFeat ]][ind.r,][!is.na(singleFeatList[[ countFeat ]][ind.r.other,])] <-
                singleFeatList[[ countFeat ]][ind.r.other, ][!is.na(singleFeatList[[ countFeat ]][ind.r.other, ])]
              #interpolate the 2 missing values using linear interpolation
              singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]] <- 
                mean(unlist(c(singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]-1], singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]+1])), na.rm=TRUE)
              
              d.X <- (singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]+1] - singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]-2])/3
              X0 <- singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]-2]    
              singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]-1] <- X0 + d.X
              singleFeatList[[ countFeat ]][ind.r,][NA_left.ind[ind.r]] <- X0 + 2*d.X
              # delete moved old vector
              singleFeatList[[ countFeat ]] <- singleFeatList[[ countFeat ]][ -ind.r.other, ]
              rownames(singleFeatList[[ countFeat ]]) <- 1:nrow(singleFeatList[[ countFeat ]])
            }   
            
            # recalculate distance matrix using updated data etc      
            NA_left.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) min(which(!is.na(x))) -1) 
            NA_right.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) max(which(!is.na(x))) ) 
            
            ind <- which(NA_left.ind == m.time )
            
            x_coord <- singleFeatList[[trackingxCoordCN]][ ind, m.time + 1 ] #current coords of short track  at t +1
            y_coord <- singleFeatList[[trackingyCoordCN]][ ind, m.time + 1 ]  
            
            #now find out where at t is a value and at t + 1 a NA
            
            ind.other <- which(NA_right.ind == (m.time-2))
            if( length(ind) !=0 & length(ind.other) != 0)
            {
              
              other_x_coords <- singleFeatList[[trackingxCoordCN]][ ind.other, m.time -2 ]
              other_y_coords <- singleFeatList[[trackingyCoordCN]][ ind.other, m.time -2  ]
              
              dist.matrix.x <- outer(x_coord, other_x_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
              dist.matrix.y <- outer(y_coord, other_y_coords, FUN = "-") # other_x_coords = columns, x_coords = rows
              dist.matrix.xy <- sqrt(dist.matrix.x^2 + dist.matrix.y^2)
              
              curr.min <- min(dist.matrix.xy[!is.na(dist.matrix.xy)],na.rm = TRUE)
            } else {
              curr.min <- 1000000 # will break while loop
            }
            
          } #  end while minDis < max recpixels
          
        } # end if indexes found block
        
      }  #m.time - loop
      if(writeAfterThirdReconnect) {
        write.table(singleFeatList[1], file = paste("trackOrderedData/beforeCombineTracks/",
                                                    currentUniqueWells[ counterStukkie],"_afterThirdTrackConnectFirstFeat.csv", sep =''), sep =",")
      }
    } # -if reconnect 3 frames
    
    
    # remove rows with too short tracks
    # find index of rows to remove and apply to all features
    
    indNotToShort <- rowSums(is.na(singleFeatList[[ 1 ]] )) <= ( ncol(singleFeatList[[ 1 ]]) - minTrackedFrames )
    
    for(countFeat in seq_along(singleFeatList))
    {
      singleFeatList[[ countFeat ]] <- singleFeatList[[ countFeat ]][indNotToShort,]
      #also renumber data.frame rows
      rownames(singleFeatList[[ countFeat ]]) <- nrow(1:nrow(singleFeatList[[1]]))
    }   
    
    displList = list()
    for ( hurryUp in 1 :( ncol(singleFeatList[[trackingxCoordCN]]) - 1 ) ){
      displList[[hurryUp]]<- sqrt( (singleFeatList[[trackingxCoordCN]][ , hurryUp + 1 ] - 
                                      singleFeatList[[trackingxCoordCN]][ , hurryUp  ])^2  +
                                     (singleFeatList[[trackingyCoordCN]][ , hurryUp + 1 ] - 
                                        singleFeatList[[trackingyCoordCN]][ , hurryUp  ])^2
      )
    }
    
    
    displList <- do.call('cbind',displList)
    displList <- cbind(rep(NA,nrow(displList)), displList)
    singleFeatList$displacement <- as.data.frame(displList)
    
    colnames(singleFeatList$displacement) <- paste("TP", 1:(max.t-min.t+1), sep ="_")
    
    #calculate directionality #lin. dist./traveled idst 
    
    NA_left.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) min(which(!is.na(x))) )
    NA_right.ind <- apply(singleFeatList[[trackingxCoordCN]], MARGIN = 1, FUN = function(x) max(which(!is.na(x))) ) 
    firstX<- singleFeatList[[trackingxCoordCN]][  , NA_left.ind ]
    lastX<- singleFeatList[[trackingxCoordCN]][  , NA_right.ind ]
    firstY<- singleFeatList[[trackingyCoordCN]][  , NA_left.ind ]
    lastY<- singleFeatList[[trackingyCoordCN]][  , NA_right.ind ]
    
    cummuDist <- rowSums(singleFeatList$displacement, na.rm = TRUE) 
    
    if(nrow(singleFeatList[[trackingxCoordCN]]) > 1 ){
      firstX<-firstX[row(firstX) == col(firstX) ]
      lastX <- lastX[row(lastX) == col(lastX) ]
      firstY<-firstY[row(firstY) == col(firstY) ]
      lastY <- lastY[row(lastY) == col(lastY) ] 
    }
    
    if(nrow(singleFeatList[[1]]) != 0) {
      directionality.data <- data.frame(directionality =   (sqrt((lastX - firstX)^2 + (lastY - firstY)^2) )/(cummuDist+1),
                                        trackLength = NA_right.ind - NA_left.ind,
                                        location = currentUniqueWells[counterStukkie],
                                        treatment = unique(myAlltrack$treatment)
      )
    } else {
      directionality.data <- data.frame()
    }
    
    # now write all 2D tables per location:
    
    if( writeSingleCellDataPerWell){
      for(countFeat in seq_along(singleFeatList))
      {
        
        naampje <- names(singleFeatList)[ countFeat]
        
        naampje.dir <- paste("trackOrderedData",naampje, sep = "/")
        if (!file.exists(naampje.dir)){
          
          dir.create(naampje.dir)
          
        }
        
        write.table(singleFeatList[[countFeat]],file =  paste(naampje.dir, "/", currentUniqueWells[ counterStukkie], ".txt", sep = '') ,  sep = "\t", col.names = NA)
        
        
      } # writing loop
    } # end if block writesinglecelldataperLocation
    
    # store data in long format
    
    
    if(nrow(singleFeatList[[1]]) != 0) {
      singleFeatList1 <- lapply(singleFeatList, function(x) {trackLabel = (1: nrow(x)) ;x <- cbind(x,trackLabel) })
      singleFeatList2 <- lapply(singleFeatList1, function(x) {location = currentUniqueWells[ counterStukkie]  ;x <- cbind(x,location) })
      singleFeatList3 <- lapply(singleFeatList2, function(x) {x <- melt(x, id.vars= c("trackLabel","location"))  })
      singleFeatList<-singleFeatList3
      rm("singleFeatList1","singleFeatList2","singleFeatList3")
      
      
      # if(!exists("allTrackDF")) {
      allTrackDF <- ldply(singleFeatList, rbind)
      # } else {
      #   allTrackDF <- rbind(allTrackDF, ldply(singleFeatList, rbind) )
      # }
    } else {
      allTrackDF = data.frame()
    }
    directionality.data.list[[ counterStukkie  ]] <- directionality.data  
    allTrackDF.list[[ counterStukkie]] <- allTrackDF
    
  } # end counterStukkie loop
  directionality.data <- rbind.fill(directionality.data.list)
  allTrackDF <- rbind.fill(allTrackDF.list)
  data.list <- list(directionality.data = directionality.data , allTrackDF = allTrackDF )
  return(data.list)
}# end fixTrackingFun
