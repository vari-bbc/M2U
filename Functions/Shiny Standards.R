
# 1.0 Logs ----

# list_obj_sizes <- function(list_obj=ls(envir=.GlobalEnv)){ 	
#     sizes <- sapply(list_obj, function(n) object.size(get(n)), simplify = FALSE) 	
#     print(sapply(sizes[order(-as.integer(sizes))], function(s) format(s, unit = 'auto'))) 
# }

# list_obj_sizes()


startSection <- function(sectionName){
  print("")
  # print("----")
  # list_obj_sizes()
  print(paste0("----", sectionName, " Start"))
}

endSection <- function(sectionName){
  # print("----")
  # list_obj_sizes()
  print(paste0("----", sectionName, " Done"))
}


# 2.0 Initial Load ----
options(shiny.maxRequestSize=300*1024^2)
options(java.parameters = "-XmX1000000m")

loadFunctions <- function(functionFileNames, functionPath = "Functions/"){
  startSection("Functions")
  
  for (i in 1:length(functionFileNames)){
    tryCatch({
      source(here::here(paste0(functionPath, functionFileNames[i])))
    }, warning = function(w) {
      print(paste0("File doesn't exist: ", functionFileNames[i]))
    })
  }
  
  endSection("Functions")
}


# 3.0 BS Theme ----
bs_theme(version = 5,primary = "#005596",preset = "cosmo",
         `enable-rounded` = TRUE)


# 4.0 Nav ----

## 4.1 Inputs and Outputs ----
navButton <- function(inputID, inputLabel){
  input_task_button(width = "100%",inputID,inputLabel,type = "default")
}

navMultiCreateSelect <- function(inputID, inputLabel){
  selectizeInput(width = "100%",inputID,inputLabel, choices = NULL,
                 multiple = T,options = list(create = T))
}

navSingleCreateSelect <- function(inputID, inputLabel){
  selectizeInput(width = "100%",inputID,inputLabel, choices = NULL,
                 multiple = F,options = list(create = T))
}

navMultiLockedSelect <- function(inputID, inputLabel){
  selectizeInput(width = "100%",inputID,inputLabel, choices = NULL,
                 multiple = T,options = list(create = F))
}

navSingleLockedSelect <- function(inputID, inputLabel){
  selectizeInput(width = "100%",inputID,inputLabel, choices = NULL,
                 multiple = F,options = list(create = F))
}

navSingleUpload <- function(inputID, inputLabel){
  fileInput(width = "100%",inputID,inputLabel, multiple = F)
}

navMultiUpload <- function(inputID, inputLabel){
  fileInput(width = "100%",inputID,inputLabel, multiple = T)
}

navDownload <- function(inputID, inputLabel){
  downloadButton(width = "100%",inputID,inputLabel)
}

navText <- function(inputID, inputLabel){
  textInput(width = "100%",inputID,inputLabel)
}

navNumeric <- function(inputID, inputLabel){
  numericInput(width = "100%",inputID,inputLabel, value = 0.05)
}

navCheckboxF <- function(inputID, inputLabel){
  input_switch(width = "100%",inputID,inputLabel, value = F)
}

navCheckboxT <- function(inputID, inputLabel){
  input_switch(width = "100%",inputID,inputLabel, value = T)
}

navColor <- function(inputID, inputLabel){
  colourInput(width = "100%",inputID,inputLabel)
}

navOutputTable <- function(outputID){
  DTOutput(outputID)
}

navOutputPlot <- function(outputID){
  plotOutput(outputID, width = "80%")
}

navOutputPic <- function(outputID){
  imageOutput(outputID, width = "80%")
}

navOutputText <- function(outputID){
  textOutput(outputID)
}


## 4.2 RMD Outputs ----
rmdNavOutputPlot <- function(thePlot){
  plotly::ggplotly(thePlot)
}

rmdNavOutputPic <- function(picPath){
  card_image(picPath, width = "80%")
}

rmdNavOutputTable <- function(theData,caption = NULL){
  datatable(theData, caption = caption, width = "100%")
}


## 4.3 Multi Element Lapply ----
multiElement <- function(elementType, elementID, elementLabel){
  #check that lengths all match
  if (length(elementType) != length(elementID) | 
      length(elementType) != length(elementLabel) |
      length(elementID) != length(elementLabel)){
    stop("elementType, elementID, and elementLabel must all be the same length")
  }
  
  lapply(1:length(elementID),function(i){
    switch(elementType[i],
      "Button" = navButton(elementID[i], elementLabel[i]),
      "MultiCreateSelect" = navMultiCreateSelect(elementID[i], elementLabel[i]),
      "SingleCreateSelect" = navSingleCreateSelect(elementID[i], elementLabel[i]),
      "MultiLockedSelect" = navMultiLockedSelect(elementID[i], elementLabel[i]),
      "SingleLockedSelect" = navSingleLockedSelect(elementID[i], elementLabel[i]),
      "SingleUpload" = navSingleUpload(elementID[i], elementLabel[i]),
      "MultiUpload" = navMultiUpload(elementID[i], elementLabel[i]),
      "Download" = navDownload(elementID[i], elementLabel[i]),
      "Text" = navText(elementID[i], elementLabel[i]),
      "Numeric" = navNumeric(elementID[i], elementLabel[i]),
      "CheckboxF" = navCheckboxF(elementID[i], elementLabel[i]),
      "CheckboxT" = navCheckboxT(elementID[i], elementLabel[i]),
      "Color" = navColor(elementID[i], elementLabel[i]),
      "OutputTable" = navOutputTable(elementID[i]),
      "OutputPlot" = navOutputPlot(elementID[i]),
      "OutputPic" = navOutputPic(elementID[i]),
      "OutputText" = navOutputText(elementID[i]),
      print(paste0(elementType[i]," is not a valid element type."))
    )
  })
}


## 4.4 Simplify Nav Parts ----
webpageTitle <- function(title){
  tags$head(tags$title(title))
}

navPadding <- function(){
  div(style = "padding: 5px 0px;")
}

navBar <- function(...){
  navset_bar(id = "root",padding = c("10px","0px"),bg = "#005596",
    ...
  )
}

navDarkSwitch <- function(mode = "dark"){
  nav_item( input_dark_mode(mode = mode) )
}

#nav_item with div added
navItem <- function(...){
  nav_item(div(style="padding: 0px 10px", ...))
} 

#nav_panel specifically for double and trip layer tabs
navSubTab <- function(title,value, ...){
  nav_panel(title,value = value, ...)
}

#navSingleTab - No sub tabs, just inputs and outputs
navSingleLevelTab <- function(title, ..., icon = NULL){
  nav_panel(title, icon = icon, card( height = cardHeight, ...))
}

#navDoubleTab - 1 layer of sub tabs
navDoubleLevelTab <- function(title, ..., icon = NULL,tripleTab = F,id = NULL){
  if (tripleTab == T){
    nav_panel(title, icon = icon, navset_card_underline( height = cardHeight, id = id,
      navItem(strong(paste0("- ",title," -"))), ...))
  } else {
  nav_panel(title, icon = icon, navset_card_underline( height = cardHeight, id = id, ...))
  }
}

#navTripleTab - contains double level tabs
navTripleLevelTab <- function(title, ..., icon = NULL){
  nav_menu(title, icon = icon, ...)
}

#navSidebar - for a sidebar
navSidebarTab <- function(title, sidebarElements = NULL, ..., icon = NULL){
  nav_panel(title = title, icon = icon, card( height = cardHeight,
    layout_sidebar(
      sidebar = sidebar(
        sidebarElements
      ),
      ...
    )
  ))
}

navSubSidebarTab <- function(title,value, sidebarElements = NULL, ..., icon = NULL){
  nav_panel(title = title, value = value, icon = icon,
    layout_sidebar(
      sidebar = sidebar(
        sidebarElements
      ),
      ...
    )
  )
}


# 5.0 Shiny Outputs ----

renderOutput <- function(theData, otherInfo = NULL){
  dataType <- class(theData)
  
  if ( dataType[1] == "character" & (sum(grepl("\\.",theData)) == length(theData)) ){
    fileSplit <- strsplit(theData, "\\.")[[1]]
    fileType <- fileSplit[length(fileSplit)]
    if (fileType %in% c("png","jpg","jpeg","svg")){
      dataType <- "pic"
    } else {
      dataType <- "Other"
    }
  }
  
  switch(dataType[1],
    "data.frame" = renderDataTable(theData, caption = otherInfo, width = "80%"),
    "gg" = renderPlot(theData),
    "pic" = renderImage(theData, width = "80%"),
    "Other" = print(paste0(dataType," is not a valid data type."))
  )
}


#6.0 Deactivate and Activate Items ----
deactivateItems <- function(itemIDs){
  for (i in 1:length(itemIDs)){
    toggleState(id = itemIDs[i], condition = F)
  }
}

activateItems <- function(itemIDs){
  for (i in 1:length(itemIDs)){
    toggleState(id = itemIDs[i], condition = T)
  }
}


# 7.0 Hide and Show Tabs ----
hideNavTabs <- function(rootID, tabIDs){
  for (i in 1:length(tabIDs)){
    nav_hide(rootID, tabIDs[i])
  }
}

showNavTabs <- function(rootID, tabIDs){
  
  for (i in 1:length(tabIDs)){
    nav_show(rootID, tabIDs[i])
  }
  nav_select(rootID, tabIDs[2])
  nav_select(rootID, tabIDs[1])
  
}
