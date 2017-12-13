#Jonathan Dixon
#Produces a table of the file name and the data infromation within
rm(list=ls()) # clear memory

RawDirectory <- "C:\\Egnyte\\Shared\\Canterbury GeoRURAL Pro\\Projects\\Jono\\Climate Data\\Raw Cliflo Data\\Rainfall\\Large rainfall extenders"

writeToDirectory <- "C:\\Egnyte\\Shared\\GeoRURAL NZ Pro\\Climate\\Checks"

rawList <-list.files(RawDirectory)


 # set up empty table

  stationTable <- data.frame(matrix(NA,nrow= length(rawList), ncol = 10), stringsAsFactors = F)
  
  
  
  rowNumber <- 1

 

for (rawStation in rawList){ #for each one
  
  
  rawFilePath <- file.path(RawDirectory,rawStation,fsep = "\\")
  
  rawData <- read.csv(rawFilePath, header = F, stringsAsFactors = F)
  
  rawThirdLine <- rawData[3,1:8]
  
  #adds file name:
  stationTable[rowNumber,1] <- rawStation
  
  stationTable[rowNumber,2:(1 + length(rawThirdLine))] <- rawThirdLine[,]  #add data to final table
  
 rowNumber <- rowNumber +1
 
}
  
names(stationTable)<- c("File Name", "Station Name", "Agent Number")

outFileName <- file.path(writeToDirectory,"Raw Data summary.csv",fsep = "\\") 

#write to the CSV#  
write.table(stationTable,outFileName,row.names=FALSE,col.names=TRUE,sep=",") 

