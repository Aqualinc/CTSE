#Jonathan Dixon
#checks through a list of files and sees if it has the matching agent number in first 9 rows

rm(list=ls()) # clear memory

rawDataDirectory <- "Q:\\Temp\\Jono\\June update\\Download updates"

noRowsMessage <- "No rows? See this help link <http://cliflo.niwa.co.nz/pls/niwp/wh.do_help?id=nodata> for possible reasons"


rawList <- list.files(rawDataDirectory)

wrongList <- character()




for (rawfile in rawList) {
  
  searchFor <- strsplit(rawfile,".csv")[[1]]
  
  rawFilePath <- file.path(rawDataDirectory,rawfile,fsep = "\\")
  
  rawFileData <- read.csv(rawFilePath,header =FALSE) #obtain raw data
  

  
  if (all(!rawFileData[1:9,]== searchFor) && (all(!rawFileData[1:9,] == noRowsMessage ))) {
    #if the seacrch for phrase cannot be found in the first 9 rows, AND doesn't contain "no rows" message:
    
    wrongList <- append(wrongList, rawfile)
  } 
  
}

if (length(wrongList) > 0){
  print(wrongList)
  
} else {
  
  print("All have matching agent number in first 9 rows or have no rows message")
}