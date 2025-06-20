
#function to remove any regions lower on the tree in places with data
removeLowerRegions <- function(theData,dataVar){
  if(dataVar %in% colnames(theData)){
    theData <- as.data.frame(theData)
    theData$numbers <- theData[,dataVar]
    
    #pull out regions with data
    partWithData <- theData[!is.na(theData$numbers),]
    highestRegions <- unique(partWithData$region)
    
    #pull out the rows of the reverse tree with those regions in it
    relevantTree <- c()
    for (i in 1:length(highestRegions)){
      search <- highestRegions[i]
      part <- tree[apply(tree, 1, function(x) any(x %in% search)),]
      relevantTree <- rbind(relevantTree,part)
    }
    inTree <- unique(relevantTree$X1)
    
    #find what rows have the region higher up
    toRemove <- inTree[!inTree %in% highestRegions]
    
    #remove the rows where it is higher
    theData <- theData[!theData$region %in% toRemove,]
    theData$numbers <- NULL
  } else {
    print("no matching data var")
  }
  
  return(theData)
}

#function to order the data for plotting
orderTheData <- function (theData,dataVar){
  
  # Begin by removing any regions below your data
  theData <- removeLowerRegions(theData,dataVar)
  
  # Pull out the shape identifier(unique) and its order
  orderKey <- theData %>% group_by(unique,order) %>% summarise()
  
  # Sort by order
  orderKey <- orderKey[order(orderKey$order),]
  
  # Ensure unique is a Factor and use the key to ensure the levels are in the right order
  theData$unique <- factor(theData$unique, levels = orderKey$unique)
  
  return(theData)
}

findFiber <- function(theData){
  
  fiberRegions <- c("fiber tracts",tree[grepl("fiber tracts",tree$path),"region"])

  theData$isFiber <- theData$region %in% fiberRegions
  return(theData)
}

findWhite <- function(theData,regions){
  
  toWhiteRegions <- c()
  for (i in 1:length(regions)){
    search <- regions[i]
    part <- c(search,tree[grepl(search,tree$path),"region"])
    toWhiteRegions <- c(toWhiteRegions,part)
  }
  
  theData$isWhite <- theData$region %in% toWhiteRegions
  return(theData)
}



