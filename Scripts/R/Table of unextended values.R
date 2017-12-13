############################################
# Jonathan Dixon  
#writes a table of all the nonempty unextended data values in a folder.
#change the fromDirectory to the converted folder (NOT extended ) and WriteToDirectory appropriately

rm(list=ls()) # clear memory

library(zoo)

#change the fromDirectory to the converted folder (NOT extended) 
FromDirectory <- "Q:\\Temp\\Jono\\June update\\Joined files"

WriteToDirectory <- "Q:\\Temp\\Jono\\June update\\"


###########################################
dealWithDuplicate <- function(fileName, DuplicatePosition, FileValueData) {
  #removes the later position of the duplicate, and prints a warning message
  FileValueData <- FileValueData[-DuplicatePosition]
  
  #Print a warning message
  print(paste("Duplicate date found in ", fileName, ", removed latter value"))
  
  return(FileValueData)
}
########################################

fileList <- list.files(FromDirectory)

#empty zoo to start with
finalTable <- zoo()

#convert to date formate to help merging issues
index(finalTable) <- as.Date(format(time(finalTable)),tz="")

columnNames <- character(length(fileList))



columnPosition <- 1


for (file in fileList){
  fileOfInterest <- file.path(FromDirectory,file,fsep = "\\")
  if (file.size(fileOfInterest)>0) {
    #obtain the Value data
  
    
    fileValueData  <-  read.csv(fileOfInterest,header=FALSE)
    
    frameDates <- as.Date(as.character(fileValueData[,1]), format = "%Y%m%d")
    #as.Date(as.character(MatchingData[,"Mydates"]),format="%Y%m%d"))
    
    fileValueData <- zoo(fileValueData[,2], order.by = frameDates)
    
    #finding duplicates
    duplicatePosition <- which(duplicated(index(fileValueData)))
    
    if(length(duplicatePosition)>0) {
      #calls function to deal with the duplicate
      fileValueData <- dealWithDuplicate(file, duplicatePosition, fileValueData)
    }
    
    
    #merge with curr final table
    finalTable <- merge(finalTable, fileValueData)
    
    #add the file name to the list
    
    columnNames[columnPosition] <- file
    
    columnPosition <- columnPosition + 1
  } else {
    #the notify the file was empty
    print(paste(file, "was empty, not included in table"))
  }
}

if (length(which(columnNames ==""))>0) {
  #remove blank name spaces
  columnNames <- columnNames[-which(columnNames =="")]
}

names(finalTable)<- columnNames

# #remove NAs
# finalTable[is.na(finalTable)] <- ""
# #finalTable <- na.fill(finalTable,"")

outFileName <- file.path(WriteToDirectory," summary unextended.csv",fsep = "\\") 

#write to the CSV#  
write.zoo(finalTable,outFileName,row.names=FALSE,col.names=TRUE,sep=",", na= "")

