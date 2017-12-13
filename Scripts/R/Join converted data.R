#Jonathan Dixon
#searches for matching converted files in the updated directory, finds any extra dates it has 
#compared to the original, and adds these. (Prioritises original data)
#Also adds any additional files

rm(list=ls()) # clear memory

library(zoo)

originalConvertedDirectory <- "Q:\\Temp\\Jono\\June update\\Test Original"

updatesConvertedDirectory <- "Q:\\Temp\\Jono\\June update\\Test update none"
  
WriteJoinedDirectory <- "Q:\\Temp\\Jono\\June update\\Test joined"



joinConvertedData <- function(originalConvertedDirectory,updatesConvertedDirectory,WriteJoinedDirectory) {
  #main coordinating function
  
  #get lists from the directories:
  
  originalList <- list.files(originalConvertedDirectory)
  

  
  updatesList <- list.files(updatesConvertedDirectory)
  
  for (originalFile in originalList){
    
    originalFilePath <- file.path(originalConvertedDirectory,originalFile,fsep = "\\")
    
    matchingFilePosition <- match(originalFile, updatesList) #in the updates list

    
    if (!is.na(matchingFilePosition)){ #if there is a matching file 
      updateFilePath <- file.path(updatesConvertedDirectory,updatesList[matchingFilePosition],fsep = "\\")
      
      #call assess data function
      mergedData <- assessData(originalFilePath,updateFilePath, matchingFilePosition)
   
    } else { 
      #use the orginal file:
      if (file.size(originalFilePath)>3) { #if not an empty file
        mergedData <- read.zoo(originalFilePath,index.column=1,sep=",",format="%Y%m%d",header=FALSE,regular=FALSE)
      } else {
        mergedData <- zoo()
      }
    }
    #write the joined data to the joined folder:
    writeToCSV(mergedData, WriteJoinedDirectory, originalFile)  
    
  }
  
  #write the additional CSVs in update list using this function (comment out if not wanted)

    writeAdditionalUpdateCSVs(updatesList, originalList, updatesConvertedDirectory,WriteJoinedDirectory)
  
}



obtainDataSets <-function(originalFilePath,updateFilePath,MatchingFilePosition){
  
  #get the data:  
  originalData <- read.zoo(originalFilePath,index.column=1,sep=",",format="%Y%m%d",header=FALSE,regular=FALSE)
  
  if (file.size(updateFilePath)>3) { #if file isn't essentially empty
    updateData  <-  read.zoo(updateFilePath,index.column=1,sep=",",format="%Y%m%d",header=FALSE,regular=FALSE)
    
  } else {
    updateData <- zoo()
  }
  
  
  
  originalAndUpdateData <- list(originalData, updateData)
  
  return(originalAndUpdateData)

}
  

  mergeDataSets <- function(OriginalplusUpdateData) {
    #merge data sets, prioritising the original
    
    OriginalData <- OriginalplusUpdateData[[1]]
    
    UpdateData <- OriginalplusUpdateData[[2]]
    
    if (length(UpdateData) >0) { #if update data isn't empty
      joinedData <-merge(OriginalData,UpdateData) #merge the data columns
      
      joinedData$singleColumn <- joinedData$OriginalData #create a new column to merge actual data
      
      indicesNA <- which(is.na(joinedData$singleColumn))# missing values in original column
      
      joinedData$singleColumn[indicesNA] <- joinedData$UpdateData[indicesNA]
      
      joinedReturned <- joinedData$singleColumn
      
    } else {
      joinedReturned <- OriginalData
    }
    
    
    
    return(joinedReturned)
    
  }
  
  assessData <- function(originalFilePath,updateFilePath, matchingFilePosition) {
    #merge data sets, prioritising the original
    if(file.size(originalFilePath)>3 && file.size(updateFilePath)>3){ #if both files aren't empty
      
      OriginalAndUpdateData <- obtainDataSets(originalFilePath,updateFilePath,matchingFilePosition)
      
      MergedData <- mergeDataSets(OriginalAndUpdateData)
    } else if(file.size(updateFilePath)>3){ #if updated file is not empty 
      
      MergedData <- read.zoo(updateFilePath,index.column=1,sep=",",format="%Y%m%d",header=FALSE,regular=FALSE)
      
    } else if(file.size(originalFilePath)>3){ #if updated file is not empty
      MergedData <- read.zoo(originalFilePath,index.column=1,sep=",",format="%Y%m%d",header=FALSE,regular=FALSE) 
      
    } else {
      MergedData <- zoo()
    }
    
    return(MergedData)
  }
  


  writeToCSV <- function(MergedData,WriteJoinedDirectory,originalFile){
    #writes the final CSV file
    
    joinedFileNamePath <- file.path(WriteJoinedDirectory,originalFile,fsep = "\\")
    if (length(MergedData)>0) { #if there's data
      
      finalData <- data.frame(matrix(NA,nrow= length(MergedData), ncol = 2))
      
      finalData[,1] <- format(index(MergedData),"%Y%m%d") #construct first with appropriate format dates
      
      finalData[,2] <- as.character(MergedData) #add the second column
      
      if (!is.numeric(MergedData)) {
        
        print(originalFile)
      }
      
  #convert to numeric
      finalData[,1] <- as.numeric(finalData[,1])
      finalData[,2] <- as.numeric(finalData[,2])
      #write to file:
      
      write.table(finalData, joinedFileNamePath,row.names=FALSE,col.names=FALSE,sep=",")
      
    } else {
      #write an empty file
      write.table(numeric(), joinedFileNamePath,row.names=FALSE,col.names=FALSE,sep=",")
    }
  }  
      

      
  writeAdditionalUpdateCSVs <- function(updatesList, originalList, updatesConvertedDirectory,WriteJoinedDirectory){
    #writes the remaining update files, not present in the original list, into the joined folder
    
    additionalUpdateFiles <- updatesList[which(is.na(match(updatesList,originalList)))]
    if (length(additionalUpdateFiles)>0) {
      for (updateFile in additionalUpdateFiles){
        #read the additional update csv:
        
        additionalUpdateFilePath <- file.path(updatesConvertedDirectory,updateFile,fsep = "\\")
        
        if (file.size(additionalUpdateFilePath)>3) { #if file isn't empty
        updateFileData <- read.csv(additionalUpdateFilePath, header = F)
        #make sure numeric rather than character
        updateFileData[,1] <- as.numeric(updateFileData[,1])
        updateFileData[,2] <- as.numeric(updateFileData[,2])
        
        } else {
          updateFileData <- numeric()
        }
        #write it into the Joined directory:
        joinedFileNamePath <- file.path(WriteJoinedDirectory,updateFile,fsep = "\\")
        
        write.table(updateFileData, joinedFileNamePath,row.names=FALSE,col.names=FALSE,sep=",")
        
      }
    }
  }
  
  
  

joinConvertedData(originalConvertedDirectory,updatesConvertedDirectory,WriteJoinedDirectory)  
