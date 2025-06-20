pretendLocalEnv <- function(){
    dontRemove <- c(mockGlobal,"mockGlobal","global","pretendLocalEnv","loadGlobals")
    rm(list = ls()[!ls() %in% dontRemove])
}

mergeData <- function(rawData, annoFile, numericCols, recAnnoCols, customAnnoCols) {
    # startSection("Merge Data Function")
    
    rawData$hemi <- NULL

    if (!"hemi" %in% colnames(annoFile)) {
        annoFile$hemi <- "right"
    }
    
    fullData <- left_join(rawData, annoFile, by = c("fileName"))
    fullData <- fullData[fullData$include == "Y",]
    fullData$include <- NULL

    # output$downloadCheckpointData <- downloadHandler(
    #     filename = function() {
    #       paste("CheckpointData.csv")
    #     },
    #     content = function(file) {
    #       disable("downloadCheckpointData")
    #       fwrite(fullData, file, row.names = FALSE)
    #     }
    # )
    # enable("downloadCheckpointData")

    # showNavTabs(rootID = "VariableSelection", 
    #             tabIDs = c("layoutSelection",
    #                        "dataTransformation",
    #                        "colorScheme",
    #                        "variableSet"))

    annoCols <- c(recAnnoCols, customAnnoCols)
    # updateSelectizeInput(session, "AoI", choices = annoCols, selected = annoCols[1])
    # updateSelectizeInput(session, "statAOI", choices = annoCols, selected = annoCols[1])
    # updateSelectizeInput(session, "customStrata", choices = customAnnoCols, selected = NULL)
    
    nutil <- any(grepl("region", numericCols, ignore.case = T) & grepl("area", numericCols, ignore.case = T))
    
    if (nutil) {
        numericCols <- numericCols[!numericCols %in% c("Load","load")]
        numericCols <- c("load", "neuriteLoad", "InclusionsPerMM2", numericCols)
        # updateSelectizeInput(session, "valueVar", choices = numericCols, selected = "load")
    } else {
        # updateSelectizeInput(session, "valueVar", choices = numericCols, selected = numericCols[1])
    }
    
    # endSection("Merge Data Function")

    return(list(fullData,numericCols))
      
}











