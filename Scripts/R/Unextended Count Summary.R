############################################
# Jonathan Dixon  
#Transform spreadsheet of extended data, the first one which ends with "unextended.csv", into Summary count format
#CHANGE startIrrYearRange to the appropriate year range(see below)

rm(list=ls()) # clear memory

#CHANGE YEAR RANGE DEPENDING ON IF THE SUMMARY IS FROM 1860 or 1960! 
#the irrigation years you want the counts to be done over:
# Irrigation year 2015 starts 1st June 2015 and ends 31st May 2016
startIrrYearRange <- 1860:2015

#where the unextended summary is:
baseDirectory <- "C:\\Users\\j.dixon\\Desktop\\Rainfall Updates\\Summaries"

summaryNameChosen <- "_unextended_count_by_irr_season.csv"




fileList <- list.files(baseDirectory)

#find the first file that has "unextendeded.csv" in it
unextendedSpreadName <-  fileList[which(grepl("unextended.csv", fileList))][1]

unextendedSpreadsheetPath <- file.path(baseDirectory,unextendedSpreadName)

#get the data
 unextendedSpreadData <- read.csv(unextendedSpreadsheetPath)

#########################################








convertAllDatesToExcelFormat <- function(unextendedSpreadData){
  
  #convert dates to character
  unextendedSpreadData[,1] <- as.character(unextendedSpreadData[,1])
  
  rowsToConvert <- grep('-', unextendedSpreadData[,1])
  
  #convert to date format
  
  datesToConvert <- as.Date(unextendedSpreadData[rowsToConvert,1])
  
  
  datesConverted <- format(datesToConvert,"%d/%m/%Y")
  
  #remove any leading zeros to be of the right format
  datesConverted <- gsub('^0','',datesConverted)
  
  #substitute back in to the big sheet
  unextendedSpreadData[rowsToConvert,1] <- datesConverted
  
  return(unextendedSpreadData)
}






calculateCount <- function(unextendedSpreadData, irrStartYear){
  #returns the count and average based on the first start date
  
  irrStartDate <- paste0("1/06/", irrStartYear)
  
  irrEndDate <- paste0("31/05/", (irrStartYear+1))
  
  irrStartDatePosition <- match(irrStartDate, unextendedSpreadData[,1])
  
  endPosition <- match(irrEndDate, unextendedSpreadData[,1])
  
  dataSubset <-  unextendedSpreadData[irrStartDatePosition:endPosition,]
  
  #count the non-Na Days for each column apart from the first column
  counts <- apply(dataSubset[,-1], 2, function(x) length(which(!is.na(x))))
  
  
  
  return(counts)
}




constructCountTable <-function(unextendedSpreadData,startIrrYearRange){
  #makes a count table of the year by year data
  #set up empty data frame
  countTable <- data.frame(matrix(NA,nrow= (ncol(unextendedSpreadData)-1), ncol = (length(startIrrYearRange)+2)))
  
  columnPosition <- 3                         
  
  for (IrrStartYear in startIrrYearRange ){ #for each year
    #run counting function
    countsInAYear <- calculateCount(unextendedSpreadData, IrrStartYear)
    
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
  
  
  agentNames <- strsplit(agentNames,".csv")
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
unextendedSpreadData <- convertAllDatesToExcelFormat(unextendedSpreadData)

#call function to construct table
CountTable <- constructCountTable(unextendedSpreadData,startIrrYearRange)

# obtain agent names
agentNamesRaw <- names(unextendedSpreadData)[-1]


CountTable <- fillInFinalTouches(CountTable, agentNamesRaw)

printTable(CountTable, baseDirectory, summaryNameChosen)


