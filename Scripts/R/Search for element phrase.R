#Jonathan Dixon
#checks through a list of files and sees if it has the matching required data type

rm(list=ls()) # clear memory

rawDataDirectory <- "C:\\Egnyte\\Shared\\Canterbury GeoRURAL Pro\\Projects\\Jono\\Climate Data\\Converted Cliflo Data\\Rainfall\\Rainfall National Joined"

searchFor <- "NA"

#find list of files
rawList <- list.files(rawDataDirectory)

foundList <- character()




for (rawfile in rawList) {
  
  rawFilePath <- file.path(rawDataDirectory,rawfile,fsep = "\\")
  
  if (file.size(rawFilePath)>0){ #if not an empty file
  
    
    
    rawFileData <- read.csv(rawFilePath,header =FALSE) #obtain raw data
    
    rawFileData[is.na(rawFileData)] <- ""   #get rid of NA values
    
    if (!all(!rawFileData== searchFor)) {#if the phrase can be found
      foundList <- append(foundList, rawfile)
    } 
  }
}

if (length(foundList) > 0){
  print(foundList)
  
} else {
  
  print("Search for phrase not found")
}