############################################
# Jonathan Dixon  
#Transform spreadsheet of extended data, the first one which ends with "extended.csv", into either the mean, max or min depending what you do
#Also select the appropriate data type to change the format, and right irrigation year range below, Check the base directory if uncommented
#Uncomment the "rm(list=ls())" and the code between the hashed lines if the post processing
#summary data (ExtendedSpreadData) isn't in the memory. In which case, check the base directory

## the following line can be uncommented if you are commenting the code between the hash lines:
#rm(list=ls()) # clear memory

#Option 1 for mean, option 2 for max, option 3 for min

 option <- 3
 
 #datatype 2 is Rain and PET which requires yearly mean rather than daily mean
 #datatype 1 is everything else
 
 dataType <- 1
 
 
#CHECK THIS: the irrigation years you want the analysis to be done over ie 1860:2015 or 1960:2015
# Irrigation year 2015 starts 1st July 2015 and ends 30th June 2016
startIrrYearRange <- 1860:2015

###################################################################

# #uncomment the code lines in this section if source isn't available:
## baseDirectory where the values extended summary is that you produced using Table of Station Values.R:
## note that this summary needs to end with " extended.csv" and the ONLY file that ends
## like this.
# 
# baseDirectory <- "C:\\Egnyte\\Shared\\Canterbury GeoRURAL Pro\\Climate\\Climate tools\\Correlation with radius model proposal\\Post Analysis\\Rain 72 48 24"
# 
# 
# fileList <- list.files(baseDirectory)
# 
# #find the first file that has " extended.csv" in it
# extendSpreadName <-  fileList[which(grepl(" extended.csv", fileList))][1]
# 
# extendSpreadsheetPath <- file.path(baseDirectory,extendSpreadName)
# 
# #get the data
# ExtendedSpreadData <- read.csv(extendSpreadsheetPath)
# 
####################################
# OTHERWISE uncomment the source if you want the post processing summary to happen :

#source("C:\\Egnyte\\Shared\\Canterbury GeoRURAL Pro\\Climate\\Climate tools\\Correlation with radius model proposal\\Post processing summary.R")


#summary name based on the option selected:

if (option == 1) {

summaryNameChosen <- "_extended_mean_by_irr_season.csv"

} else if (option == 2) {
  
  summaryNameChosen <- "_extended_max_by_irr_season.csv"
  
} else if (option == 3) {
  
  summaryNameChosen <- "_extended_min_by_irr_season.csv"
  
} else {
  
  print("Invalid option chosen")
}




convertAllDatesToExcelFormat <- function(extendedSpreadData){
  
  #convert dates to character
  extendedSpreadData[,1] <- as.character(extendedSpreadData[,1])
  
  rowsToConvert <- grep('-', extendedSpreadData[,1])
  
  #convert to date format
  
  datesToConvert <- as.Date(extendedSpreadData[rowsToConvert,1])
  
  
  datesConverted <- format(datesToConvert,"%d/%m/%Y")
  
  #remove any leading zeros to be of the right format
  datesConverted <- gsub('^0','',datesConverted)
  
  #substitute back in to the big sheet
  extendedSpreadData[rowsToConvert,1] <- datesConverted
  
  return(extendedSpreadData)
}






calculateAnalysis <- function(ExtendedSpreadData, irrStartYear, option, dataType){
  #returns the row of analysis based on the first start date
  
  irrStartDate <- paste0("1/06/", irrStartYear)
  
  irrEndDate <- paste0("31/05/", (irrStartYear+1))
  
  irrStartDatePosition <- match(irrStartDate, ExtendedSpreadData[,1])
  
  endPosition <- match(irrEndDate, ExtendedSpreadData[,1])
  
  dataSubset <-  ExtendedSpreadData[irrStartDatePosition:endPosition,]
  
  #an adjuster depending of if this is PET or Rain datatype
  if (dataType == 1) {
    adjuster <- 1
    } else if (dataType == 2) {
      adjuster <- 365.25
    }
  
  
  if (option == 1){
    #perform mean analysis on the non-Na Days, if there are more than 360 days,  for each column apart from the first column
    #for this option an adjuster is included
    analysisRows <- apply(dataSubset[,-1], 2, function(x) if (length(which(!is.na(x)))>=360) {adjuster*mean(x, na.rm = TRUE)} else {NA})
    
  } else if (option == 2){
    #perform Max analysis on the non-Na Days, if there are more than 360 days,  for each column apart from the first column
    analysisRows <- apply(dataSubset[,-1], 2, function(x) if (length(which(!is.na(x)))>=360) {max(x, na.rm = TRUE)} else {NA})
    
  } else if (option == 3){
    #perform Min analysis on the non-Na Days, if there are more than 360 days,  for each column apart from the first column
    analysisRows <- apply(dataSubset[,-1], 2, function(x) if (length(which(!is.na(x)))>=360) {min(x, na.rm = TRUE)} else {NA})
    
  } 
  
  
  
  
  return(analysisRows)
}




constructAnalysisTable <-function(ExtendedSpreadData,startIrrYearRange,option, dataType){
  #makes a Analysis table of the year by year data
  
  if (option == 1){ #different number of columns depending on which option selected
    extraColumns <- 1
  } else if (option ==2 || option == 3) {
    extraColumns <- 2
  }
  
  
  #set up empty data frame
  analysisTable <- data.frame(matrix(NA,nrow= (ncol(ExtendedSpreadData)-1), ncol = (length(startIrrYearRange)+extraColumns)))
  
  
  
  #fill in after the extra columns
  columnPosition <- extraColumns + 1                         
  
  for (IrrStartYear in startIrrYearRange ){ #for each year
    #run analysis function
    analysisInAYear <- calculateAnalysis(ExtendedSpreadData, IrrStartYear,option, dataType)
    
    #add it to the table
    analysisTable[,columnPosition] <- analysisInAYear
    
    columnPosition <- columnPosition +1
    
  }
  
  #if it's the mean option for Rain or PET type data:
  if(dataType == 2 && option == 1){
    rounding <- 0
  } else {
    rounding <- 1
  }
    
  
  
  #round table to the appropriate amount:
  analysisTable <- data.frame(lapply(analysisTable, round, rounding))

  
  endYear <- as.character(startIrrYearRange +1)
  
  #take the third and fourth characters:
  endYearReduced <- substr(endYear,3,4)
  
  #the resulting names:
  yearColumnNames <- paste0(startIrrYearRange, "_",endYearReduced)
  
  if (option == 1){ #choosing extra columns
  
    firstTitles <- c("Agent")
        
    if(dataType == 2){ #if it's the rainfall PET datatype, change the title
      firstTitles <- c("Agent.Yearly Average")
      
    }
    
  
  } else if (option == 2){
    firstTitles <- c("Agent", "Max post 1960")
    
  } else if (option == 3){
    firstTitles <- c("Agent", "Min post 1960")
  }
  
  
  
  names(analysisTable) <- append(firstTitles,yearColumnNames)
  
  
  
  
  return(analysisTable)
}






fillInFinalTouches <-function(analysisTable, agentNames, option) {
  if (option ==2 || option ==3){
    
    column1960 <- which("1960_61" ==names(analysisTable)) #position of 1960 column
    
    subsetPost1960 <- analysisTable[,column1960:ncol(analysisTable)]
    
    if (option ==2){
      #find the Maximum for that years post 1960 if there are more than 50 calculated maximums in that time
      summaryValue <- apply(subsetPost1960, 1, findMaxPost1960)
    } else if (option == 3) {
      #find the Minimum for that years post 1960 if there are more than 50 calculated maximums in that time
      summaryValue <- apply(subsetPost1960, 1, findMinPost1960)
    }
    
    #put the summary values in the analysis table
    analysisTable[,2] <- summaryValue
  }
  

  #get headers, remove excess to get agent names
  
  
  agentNames <- strsplit(agentNames,".extended.csv")
  #remove the X
  agentNames <- substring(agentNames, 2)
  
  #fill in the remaining row
  analysisTable[,1] <-agentNames

  
  return(analysisTable)
}



findMaxPost1960 <- function(Row) {
  #finds the maximum value post 1960 if there are more than 50 years
  numberYearsPost1960 <- length(which(!is.na(Row)))
  
  if (numberYearsPost1960 >50) {
    summaryValue <- max(Row, na.rm =TRUE)
  }  else {
    summaryValue <- NA
  } 
}


findMinPost1960 <- function(Row) {
  #finds the maximum value post 1960 if there are more than 50 years
  numberYearsPost1960 <- length(which(!is.na(Row)))
  
  if (numberYearsPost1960 >50) {
    summaryValue <- min(Row, na.rm =TRUE)
  }  else {
    summaryValue <- NA
  } 
}




printTable <-function(finalAnalysisTable, baseDirectory, summaryNameChosen){
  
  printFilePath <- file.path(baseDirectory, summaryNameChosen, fsep = "\\")
  
  write.csv(finalAnalysisTable,printFilePath, na = "", row.names = FALSE)
  
  
}

#convert to appropriate format
ExtendedSpreadData <- convertAllDatesToExcelFormat(ExtendedSpreadData)

#call function to construct table
analysisTable <- constructAnalysisTable(ExtendedSpreadData,startIrrYearRange, option, dataType)

# obtain agent names
agentNamesRaw <- names(ExtendedSpreadData)[-1]


analysisTable <- fillInFinalTouches(analysisTable, agentNamesRaw, option)

printTable(analysisTable, baseDirectory, summaryNameChosen)
