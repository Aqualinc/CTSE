############################################
# Jonathan Dixon  
#writes a table of all the extended data origins in a folder

rm(list=ls()) # clear memory
#where your extended files are:
FromDirectory <- "C:\\Users\\j.dixon\\Desktop\\Rainfall Updates\\Extended"

#where you want it written to:
WriteToDirectory <- "C:\\Users\\j.dixon\\Desktop\\Rainfall Updates\\Summaries"

fileList <- list.files(FromDirectory)

sampleFile <- fileList[1]

sampleFileAddress <- file.path(FromDirectory,sampleFile,fsep = "\\")

#obtain dates data from first (sample) file
datesData <- read.csv(sampleFileAddress,header=TRUE)[1]






#preallocate data frame:

finalTable <- data.frame(matrix(NA,nrow= nrow(datesData), ncol = (length(fileList)+1)))

#input dates Data
finalTable[,1] <- datesData

colnames(finalTable)[1] <- "Date"


columnPosition <- 2

for (file in fileList){
  fileOfInterest <- file.path(FromDirectory,file,fsep = "\\")
  
  #obtain the data
  readData  <-  read.csv(fileOfInterest,header=TRUE)
  
  #origin is in the 3rd column:
  originData <- readData[,3]
  
  
  finalTable[,columnPosition] <- originData
  
  #name the column
  colnames(finalTable)[columnPosition] <- file
  
  columnPosition <- columnPosition + 1
  
}

#remove NAs
finalTable[is.na(finalTable)] <- ""


outFileName <- file.path(WriteToDirectory,"Origins extended summary.csv",fsep = "\\") 

#write to the CSV#  
write.table(finalTable,outFileName,row.names=FALSE,col.names=TRUE,sep=",")
