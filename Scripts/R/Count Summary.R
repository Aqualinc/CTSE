############################################
# Jonathan Dixon  
#Transform spreadsheet of extended data, the first one which ends with "extended.csv", into Summary count format
#CHANGE startIrrYearRange to the appropriate year range(see below)
#check the source is right if you're using that option, otherwise uncomment the section between
#the hash lines and check that that's right

rm(list=ls()) # clear memory



# sourcing the Post processing summary to get it running all in one:
# check that the base directory in that program is right, and the source address

source("C:\\Egnyte\\Shared\\GeoRURAL NZ Pro\\Climate\\Programming\\Post processing summary2.R")


#CHANGE YEAR RANGE DEPENDING ON IF THE SUMMARY IS FROM 1860 or 1960! 
# #the years you want the counts to be done over. Irrigation year 2015 starts 1st June 2015 and
# #ends 31st May 2016
# 
 startIrrYearRange <- 1960:2015


summaryNameChosen <- "_extended_count_by_irr_season.csv"





######################################
# 
# uncomment following lines if you can't source the Post processing summary.R above (eg. source(..))
# 
# baseDirectory <- "C:\\Egnyte\\Shared\\Canterbury GeoRURAL Pro\\Climate\\Climate tools\\Correlation with radius model proposal\\Post Analysis\\Rain 72 48 24"
# 
# 
# fileList <- list.files(baseDirectory)
# 
# #find the first file that has "extended.csv" in it
# extendSpreadName <-  fileList[which(grepl("extended.csv", fileList))][1]
# 
# extendSpreadsheetPath <- file.path(baseDirectory,extendSpreadName)
# 
# #get the data
# ExtendedSpreadData <- read.csv(extendSpreadsheetPath)

# 
# 
# #########################################








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






calculateCount <- function(ExtendedSpreadData, irrStartYear){
  #returns the count and average based on the first start date
  
  irrStartDate <- paste0("1/06/", irrStartYear)
  
  irrEndDate <- paste0("31/05/", (irrStartYear+1))
  
  irrStartDatePosition <- match(irrStartDate, ExtendedSpreadData[,1])
  
  endPosition <- match(irrEndDate, ExtendedSpreadData[,1])
  
  dataSubset <-  ExtendedSpreadData[irrStartDatePosition:endPosition,]
  
  #count the non-Na Days for each column apart from the first column
  counts <- apply(dataSubset[,-1], 2, function(x) length(which(!is.na(x))))
  
  
  
  return(counts)
}




constructCountTable <-function(ExtendedSpreadData,startIrrYearRange){
  #makes a count table of the year by year data
  #set up empty data frame
  countTable <- data.frame(matrix(NA,nrow= (ncol(ExtendedSpreadData)-1), ncol = (length(startIrrYearRange)+2)))
  
  columnPosition <- 3                         
  
  for (IrrStartYear in startIrrYearRange ){ #for each year
    #run counting function
    countsInAYear <- calculateCount(ExtendedSpreadData, IrrStartYear)
    
    #add it to the table
    countTable[,columnPosition] <- countsInAYear
    
    columnPosition <- columnPosition +1
    
  }
  
  endYear <- as.character(startIrrYearRange +1)
  
  #take the third and fourth characters:
  endYearReduced <- substr(endYear,3,4)
  
  #the resulting names:
  yearColumnNames <- paste0(startIrrYearRange, "_",endYearReduced)
  
  firstTwoNames <- c("Agent", "Years")
  
  names(countTable) <- append(firstTwoNames,yearColumnNames)
  
  
  return(countTable)
}

yearCount <- function(countedTable){
  
  

  rete
  
}
  



fillInFinalTouches <-function(CountedTable, agentNames) {
  
  #performs year count on the table for those years >= 360 days in it (apart from first 2 columns obviously)
  countYears <- apply(CountedTable[,-c(1,2)], 1, function(x) length(which(x >=360)))
  
  #get headers, remove excess to get agent names
  
  
  agentNames <- strsplit(agentNames,".extended.csv")
  #remove the X
  agentNames <- substring(agentNames, 2)
  
  #fill in the remaining two rows
  CountedTable[,1] <-agentNames
  
  CountedTable[,2] <-countYears
  
  return(CountedTable)
}



printTable <-function(finalCountTable, baseDirectory, summaryNameChosen){
  #prints the CSV to the base direcotry
  printFilePath <- file.path(baseDirectory, summaryNameChosen, fsep = "\\")
  
  write.csv(finalCountTable,printFilePath, na = "", row.names = FALSE)
  
  
  }

#convert to appropriate format
ExtendedSpreadData <- convertAllDatesToExcelFormat(ExtendedSpreadData)

#call function to construct table
CountTable <- constructCountTable(ExtendedSpreadData,startIrrYearRange)

# obtain agent names
agentNamesRaw <- names(ExtendedSpreadData)[-1]


CountTable <- fillInFinalTouches(CountTable, agentNamesRaw)

printTable(CountTable, baseDirectory, summaryNameChosen)


