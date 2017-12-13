#Jonathan Dixon
#converts rainfall data and removes data other than 24 hours depending on what option you select. Also removes the
#latter of any duplicate dates. Select the option you prefer. Change the directories to where
#you want them:


rm(list=ls()) # clear memory

library (zoo)
#USER! select option you prefer:
#option <- 1 for removing all days other than 24

#option <- 2 for removing all days other than 24, 48, 72. For 48 and 72, it averages it over the current day and the previous 
# one or two days respectively. If this would create a double up of values for a date (which you would expect it shouldn't),
# then the observation is discarded. 

option <- 2

  
#Specify the directory that holds the raw files that need to be reformatted
FromDirectory <- "Q:\\Temp\\Jono\\June update\\Download updates"

#specify the directory to write the formatted files to
ToDirectory <- "Q:\\Temp\\Jono\\June update\\Converted Files"

#directory if non-existent data
FaultyDirectory <- "Q:\\Temp\\Jono\\June update\\Converted Files"

ReportDirectory <-  "Q:\\Temp\\Jono\\June update"
  
  
  writeAndReport <- function(Data, fileName,option){
    
    Data <- Data[which(!Data[,5]=="-"),]
    
    if(ncol(Data) > 8){#if the data is wide enough to be relevant
    
      amountDataToRemove <- nrow(Data[which(!Data[,9]== 24),])
      
      if (option ==2) {
        
        #call the function to deal with the 7248 data
        Data <- dealWith7248(Data,fileName)
        
      }
      
      
      
      rowsToRemove <- which(!Data[,9]== 24)
        
      if( length(rowsToRemove)>0 ){
        #Then remove rows without 24
        Data <-  Data[-rowsToRemove,]
      }
      
      
      
      #then write the file
      outFileName <- file.path(ToDirectory,file,fsep = "\\")
      ColumnsOfInterest <- c(3,5)
      
      numericData <- Data[,ColumnsOfInterest]
      #convert to numeric:
      
      
   #   numericData <- as.matrix(sapply(numericData, as.numeric))
       numericData[,1] <- as.numeric(as.character(numericData[,1]))#to ensure correct conversion requires to 
   # be character first for some cases?
       
   
       numericData[,2] <- as.numeric(as.character(numericData[,2])) #to ensure correct conversion requires to 
      # be character first for some cases?
       
      #search for duplicates
       duplicatePosition <- which(duplicated(numericData[,1]))
       
       
       if(length(duplicatePosition)>0) {
         
         for (positionPlace in 1:length(duplicatePosition)){
           print(paste("duplicate date found in",fileName, "at", numericData[duplicatePosition[positionPlace],1], ". Will remove the latter one."))
           
  
         }
         #remove later position of duplicates
         numericData <- numericData[-duplicatePosition,]
       }
       
       #remove NAs
       numericData <- numericData[complete.cases(numericData),]
       
      #write the table
      write.table(numericData,outFileName,row.names=FALSE,col.names=FALSE,sep=",", na = "")
    } else{
      #data irrelevation, no data to remove
      amountDataToRemove <- 0
    }
   
    return(amountDataToRemove)
  }
  
  
  dealWith7248 <- function(dataSet,FileName) {
    #convert to zoo, get date in format
    frameDates <- as.Date(as.character(dataSet[,3]), format = "%Y%m%d")
    
    dataZoo <- zoo(dataSet, order.by = frameDates)
    
    #deal with 48 hour data:
    dataSet <- insertNewData(dataSet, dataZoo, observationLength = 48, FileName)
    
    #deal with 72 hour data:
    dataSet <- insertNewData(dataSet, dataZoo, observationLength = 72, FileName)
    

    return(dataSet)
  }
    
  
  insertNewData <- function(DataSet, DataZoo, observationLength, filename){
    
    #those days that are the observation Length observations in question:
    observationSet <- DataSet[which(DataSet[,9]== observationLength),]
    
    if (nrow(observationSet)>0){#if there are any of the relevant observations:
    
      frameDates <- as.Date(as.character(observationSet[,3]), format = "%Y%m%d")
      
      observationSet <- zoo(observationSet, order.by = frameDates)
      
      for (rowNumber in 1: nrow(observationSet)){
        #take the observation row:
        observation <- observationSet[rowNumber,]
        
        
        firstCreatedDate <- index(observation) - (observationLength/24 -1)
        #generate sequence of created dates
        createdDatesSequence <- seq(firstCreatedDate, index(observation), by = "day")
        
        
        spreadObservations <- data.frame(matrix(NA,nrow= length(createdDatesSequence), ncol = length(observation)))
        
        #fill each line with the original observation
        spreadFill <- t(apply(spreadObservations, 1, function(x) observation ))
        
        
        #put in dates
        spreadFill[, 3] <-  as.character(createdDatesSequence, format = "%Y%m%d")
        
        #put in averaged values over number of days, 24 in the frequency column and an extra comment:
        spreadFill[, 5] <- as.numeric(spreadFill[, 5])/(observationLength/24)
        
        spreadFill[, 9] <- 24
        
        spreadFill[, 10] <- paste("from",observationLength)
        
        
        lastDate <- spreadFill[nrow(spreadFill),3]
        
        #find the date in the total dataSet
        dataPosition <- which(DataSet[,3]== lastDate)
        
        #searching for date double ups:
        dateDoubleUps <- match(spreadFill[1:(nrow(spreadFill)-1),3],DataSet[,3])
        
        if(all(is.na(dateDoubleUps))){
          #if all dates to be inserted, apart from the final one, are not found in the data set (they shouldn't be!)
        
        #put in the data before, insert adjusted data, then add the rest
          newDataSet <- DataSet[1:(dataPosition-1),]
          
        #make column names compatible:  
          colnames(spreadFill) <- colnames(newDataSet)
          
          newDataSet2 <- rbind.data.frame(newDataSet, spreadFill)
          
          newDataSetFull <- rbind.data.frame(newDataSet2, DataSet[(dataPosition+1):nrow(DataSet),])
          
          DataSet <- newDataSetFull
          
        } else {
          
          doubleUpDatePosition <- dateDoubleUps[!is.na(dateDoubleUps)]
          
          doubleUpDate <- DataSet[doubleUpDatePosition,3]
          
          print(paste("In", filename, "file, a double up would have occurred at date", doubleUpDate))
          
          #and then the data point is left to be removed
          
        }
        
      }
    }
    return(DataSet)
  }
  
  
  
  
  printReport <- function(RemovalReport, ReportDirectory,option){
    
    names(RemovalReport) <- c("File", "Number of days removed")
    
    outFileName <- file.path(ReportDirectory,"Rows removed report.csv",fsep = "\\") 
    
    #write to the CSV#  
    write.table(RemovalReport,outFileName,row.names=FALSE,col.names=TRUE,sep=",", na="")
    
  }
  
  
  #get the list of files that need reformatting
  fileList <- list.files(FromDirectory)
  



  #create empty data frame
  removalReport <- data.frame(matrix(NA,nrow= length(fileList), ncol = 2))
  reportRow <- 1
  
  for (file in fileList){
    fileOfInterest <- file.path(FromDirectory,file,fsep = "\\")
    #if the file is not empty
    if (length(readLines(fileOfInterest)) > 0){
      #read the data
      data  <-  read.csv(fileOfInterest,header=FALSE,skip=9,nrow=length(readLines(fileOfInterest))-15)
      

      #remove rows containing NA
      data <- data[complete.cases(data),]
      
      if (is.data.frame(data)){
        
        amountDataToRemove <- writeAndReport(data, file,option)
        
        #add amount of rows removed to the report
        removalReport[reportRow,] <- c(file, amountDataToRemove )
        
        reportRow <- reportRow +1
        
      }
      else {
        outFileName <- file.path(FaultyDirectory,file,fsep = "\\")
        write.table(0,outFileName,row.names=FALSE,col.names=FALSE,sep=",")
        
      }
    } 
    else {
        outFileName <- file.path(FaultyDirectory,file,fsep = "\\")
        write.table(0,outFileName,row.names=FALSE,col.names=FALSE,sep=",")
    
    }

  }
  
  #IF you want a report, print the following
  printReport(removalReport, ReportDirectory,option)
  