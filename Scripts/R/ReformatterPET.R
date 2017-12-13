#Jonathan Dixon
#Converts min data and also removes the
#latter of any duplicate dates. Select the option you prefer. Change the directories to where
#you want them:


rm(list=ls()) # clear memory

#Specify the directory that holds the files that need to be reformatted
FromDirectory <- "C:\\Users\\pbrow_000\\Desktop\\PET updating\\update PET downloads"

#specify the directory to write the formatted files to
ToDirectory <- "C:\\Users\\pbrow_000\\Desktop\\PET updating\\update PET converted"

#directory if non-existent data
FaultyDirectory <- "C:\\Users\\pbrow_000\\Desktop\\PET updating\\update PET faulty"


dealWithDuplicateDates<- function(numericData, fileName){
  #removes the latter of any dupilcate dates
  
  #find any duplicated dates
  duplicatePosition <- which(duplicated(numericData[,1]))
  
  if(length(duplicatePosition)>0) {
    
    for (positionPlace in 1:length(duplicatePosition)){
      print(paste("duplicate date found in",fileName, "at", numericData[duplicatePosition[positionPlace],1], ". Will remove the latter one."))
      
      
    }
    #remove later position of duplicates
    numericData <- numericData[-duplicatePosition,]
  }
  return(numericData)
}








#get the list of files that need reformatting
fileList <- list.files(FromDirectory)

for (file in fileList){
  fileOfInterest <- file.path(FromDirectory,file,fsep = "\\")
  #if the file is not empty
  if (length(readLines(fileOfInterest)) > 0){
    #get the data from it
    data  <-  read.csv(fileOfInterest,header=FALSE,skip=9,nrow=length(readLines(fileOfInterest))-17)
    # remove NA values:
    data[is.na(data)] <- ""
    if (ncol(data)>2){ #if 3 or more columns present
      #eliminate those rows with dashes in 5th column, ADJUST THE COLUMN IF NEEDED!
      data <- data[which(!data[,5]=="-"),]
      
      #then prepare the file
      outFileName <- file.path(ToDirectory,file,fsep = "\\")
      ColumnsOfInterest <- c(3,5)
      
      convertedData <- data[,ColumnsOfInterest]
      
      #deal with duplicates using function:
      noDuplicatesData <- dealWithDuplicateDates(convertedData, file)
      
      write.table(noDuplicatesData,outFileName,row.names=FALSE,col.names=FALSE,sep=",")
    }
    else {
      #write an empty file
      outFileName <- file.path(FaultyDirectory,file,fsep = "\\")
      file.create(outFileName)
      
    }
  } 
  else {
    outFileName <- file.path(FaultyDirectory,file,fsep = "\\")
    file.create(outFileName)
  }
  
}




