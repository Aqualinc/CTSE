#Jonathan Dixon
#checks through a list of files and sees if it has the matching required data type

rm(list=ls()) # clear memory

rawDataDirectory <- "Q:\\Temp\\Jono\\June update\\Download updates"

#put in the trademark search for element, exactly (no more no less) in for the searchFor phrase
searchFor <- "Rain: Daily"


rawList <- list.files(rawDataDirectory)

wrongList <- character()




for (rawfile in rawList) {
  
  rawFilePath <- file.path(rawDataDirectory,rawfile,fsep = "\\")
  
  rawFileData <- read.csv(rawFilePath,header =FALSE) #obtain raw data
  

  
  if (all(!rawFileData[1:9,]== searchFor)) {#if the seacrch for phrase cannot be found in the first 9 rows
    wrongList <- append(wrongList, rawfile)
  } 
  
}

if (length(wrongList) > 0){
  print(wrongList)
  
} else {
  
  print("All have searchFor phrase in first 9 rows")
}