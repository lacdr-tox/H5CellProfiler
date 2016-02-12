countCellFun <- function(kColNames )
{
  
  
  cellNlist = list()  
  jumpInd <-length(uniqueLocations) %/% numberCores
  uniqueLocationLevels <- rep(1:numberCores, each = jumpInd)
  #add some extra at the end in case levels is shorter:
  extraEnd <-  length(uniqueLocations) - length(uniqueLocationLevels) 
  uniqueLocationLevels<- c(uniqueLocationLevels,  rep(uniqueLocationLevels[length(uniqueLocationLevels)], extraEnd))
  if(length(uniqueLocationLevels) != length(uniqueLocations) | !all(sort(uniqueLocationLevels) == uniqueLocationLevels)) {
    stop("making uniqueLocationLevels failed")
  }
  uniqueLocationGroups  = list()
  for(countergroups in seq_along(unique(uniqueLocationLevels))) {
    uniqueLocationGroups[[countergroups]] <- uniqueLocations[ uniqueLocationLevels == countergroups]
  }
  
  # divide data in parts to avoid copying entire object nCore times...
  setkey(myDFo, plateWellID)
  splitDataL =list()
  for(splitLoop in seq_along(uniqueLocationGroups) )
  {
    splitDataL[[splitLoop]] <- myDFo[uniqueLocationGroups[splitLoop]]
  }
  
  print("Calculating min & max parent object counts:")
  #kColNames$ImageCountParentsCN <- "imageCountParentObj"
  cl<-makeCluster(numberCores)
  registerDoSNOW(cl)
  cellNlist <- foreach ( cellC = splitDataL, .packages = "data.table") %dopar% 
{
  parentCounts <- cellC[ , list(minCountParent = min(
    get(kColNames$ImageCountParentsCN), na.rm=TRUE),
    maxCountParent = max(
      get(kColNames$ImageCountParentsCN), na.rm=TRUE),
    meanCountParent = round(
      mean(get(kColNames$ImageCountParentsCN), na.rm=TRUE), digits= 1)), by = plateWellID]
}
  
stopCluster(cl)

cellNlist <- rbindlist(cellNlist)
write.table(cellNlist,file = "parentObjectCounts.txt", sep ="\t", row.names = FALSE)
return(splitDataL)
} 

