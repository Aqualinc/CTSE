#Jonathan Dixon
#check for duplicate dates in data

rm(list=ls()) # clear memory

SearchDirectory <- "C:\\Egnyte\\Shared\\GeoRURAL NZ Pro\\Climate\\Rainfall\\Cliflo converted\\Rainfall National 72 48 24"

fileList <- list.files(SearchDirectory)

#fileName <- "1340.csv"
searchForDuplicates <- function(fileName) {

  fileOfInterest <- file.path(SearchDirectory,fileName,fsep = "\\")
  
  #if the file is not empty
  if (length(readLines(fileOfInterest)) > 0){
    #read the data
    Data  <-  read.csv(fileOfInterest,header=FALSE)
    
    
    #remove rows containing NA
    #Data <- Data[complete.cases(Data),]
    
    duplicatePosition <- which(duplicated(Data[,1]))
    
    if(length(duplicatePosition)>0) {
      
      duplicateDate <- Data[duplicatePosition,1]
      
      print(paste(fileName, "has a duplicate date", duplicateDate))    
  
    }
  }
}

lapply(fileList, searchForDuplicates)