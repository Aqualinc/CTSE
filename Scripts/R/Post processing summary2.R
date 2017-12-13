############################################
# Jonathan Dixon  
#Transform spreadsheet of extended data, the first one which ends with "extended.csv", into Summary format
# Change the base directory to where the extended summary is:

rm(list=ls()) # clear memory

#datatype 2 is Rain and PET which requires yearly mean rather than daily mean
#datatype 1 is everything else

dataType <- 2

# #CHANGE YEAR RANGE DEPENDING ON IF THE SUMMARY IS FROM 1860 or 1960! 
# #the years you want the counts to be done over. Irrigation year 2015 starts 1st June 2015 and
# #ends 31st May 2016
startIrrYearRange <- 1960:2015

#where the values extended summary is that you produced using Table of Station Values.R:
#note that this summary needs to end with " extended.csv" and the ONLY file that ends
#like this.
baseDirectory <- "C:\\Users\\j.dixon\\Desktop\\PET\\extension process\\PET update extension files\\Summaries"
#amount of missing days post 1972 allowed in the summary
days1972Tolerance <- 10

summaryNameChosen <- "_Summary.csv"



fileList <- list.files(baseDirectory)

#find the first file that has " extended.csv" in it
extendSpreadName <-  fileList[which(grepl(" extended.csv", fileList))][1]

extendSpreadsheetPath <- file.path(baseDirectory,extendSpreadName)

#get the data
ExtendedSpreadData <- read.csv(extendSpreadsheetPath)



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



calculateAndOrganise <- function(extendedSpreadData, dataType, startIrrYearRange){
  #calculates relevant values from the spreadsheet and puts them in a table
  #collect results from the data for different dates
  
  finalIrrigationYear <- tail(startIrrYearRange, n=1)
  
  #end date of final irration year
  latestIrrEndDate <- paste0("31/05/",(finalIrrigationYear+1))
  
  latestEndDate <- tail(extendedSpreadData[,1], n=1)
  
  from1960Data <- calculateCountAverage(extendedSpreadData, irrStartDate = "1/06/1960", irrEndDate = latestIrrEndDate, dataType)
  
  from1972Data <- calculateCountAverage(extendedSpreadData, irrStartDate = "1/06/1972", irrEndDate = latestIrrEndDate, dataType)
  
  from1960to85Data <- calculateCountAverage(extendedSpreadData, irrStartDate = "1/01/1960", irrEndDate = "31/12/1985", dataType)
  
  from1990toLatestData <- calculateCountAverage(extendedSpreadData, irrStartDate = "1/01/1990", irrEndDate = latestEndDate, dataType)
  
  #get headers, remove excess to get agent names
  agentNames <- names(extendedSpreadData)[-1]
  
  agentNames <- strsplit(agentNames,".extended.csv")
  #remove the X
  agentNames <- substring(agentNames, 2)
  
  
  #set up a table to put it all in
  calculateTable <- data.frame(matrix(NA,nrow= (ncol(extendedSpreadData)-1), ncol = 9))
  
  #adjust a title based on datatype
  if (dataType == 1) {
    periodFrom1972TitlePart <- "Day Average"
    
    januaryTitle <- "January daily avg from 1972"
    
    julyTitle <- "July daily avg from 1972"
    
  } else if (dataType == 2) {
    periodFrom1972TitlePart <- "Yearly Average"
    
    januaryTitle <- "January Monthly avg from 1972"
    
    julyTitle <- "July Monthly avg from 1972"
  }
  
  periodFrom1972Title <- paste(periodFrom1972TitlePart,"irrYears 1972 to", finalIrrigationYear)
  
  countfrom1972T <- paste0("Count_Irryears 1972to", finalIrrigationYear)
  
  countfrom1960T <- paste0("Count_Irryears 1960to", finalIrrigationYear)
  
  normal60_85 <- "Avg1960_1985 Jan-Dec"
  
  Jan1990toLatest <- "Avg1990Jan to LatestExtended"
  
  
  names(calculateTable)<- c("Agent", countfrom1972T, countfrom1960T, periodFrom1972Title, januaryTitle, julyTitle, normal60_85, Jan1990toLatest, "Change")
  
  #fill in the columns
  calculateTable$Agent <- agentNames
  
  calculateTable[,2] <- from1972Data$counts
  
  calculateTable[,3] <- from1960Data$counts
  
  #rounded to 1dp, or no dp depending on data type:
  if (dataType ==1) {
    
    averageRounding <- 1
  } else if (dataType ==2) {
    averageRounding <- 0
  }
  
  #do the rounding:
  
  calculateTable[,4] <- round(from1972Data$average,averageRounding)
  
  calculateTable[,5] <- round(from1972Data$januaryAverage,1)
  
  calculateTable[,6] <- round(from1972Data$julyAverage,1)
  
  #rounded to 2 dp
  calculateTable[,7] <- round(from1960to85Data$average,2)
  
  calculateTable[,8] <- round(from1990toLatestData$average,2)
  
  calculateTable$Change <- round((from1990toLatestData$average- from1960to85Data$average),2)
  
  return(calculateTable)
  
}

calculateCountAverage <- function(ExtendedSpreadData, irrStartDate, irrEndDate, dataType){
  #returns the count and average based on the first start date


  
  irrStartDatePosition <- match(irrStartDate, ExtendedSpreadData[,1])
  
  endPosition <- match(irrEndDate, ExtendedSpreadData[,1])
  
  dataSubset <-  ExtendedSpreadData[irrStartDatePosition:endPosition,]
  
  #count the non-Na Days for each column apart from the first column
  counts <- apply(dataSubset[,-1], 2, function(x) length(which(!is.na(x))))
  
  if (dataType == 1) {
    #don't adjust for data type 1:
    adjuster <- 1
    
  } else if (dataType ==2) {
    #adjust to a yearly amount
    adjuster <- 365.25
    
  } else {
    print("Invalid dataType selected")
  }
  
  averageAdjusted <- adjuster*colMeans(dataSubset[,-1], na.rm = TRUE)
  
  
  if (irrStartDate == "1/06/1972") {
    
    if (dataType == 1) {
      #don't adjust for data type 1:
      monthlyOrNotAdjuster <- 1
      
    } else if (dataType ==2) {
      #adjust to a monthly amount
      monthlyOrNotAdjuster <- 31
      
    }
    
    
    
    januarySubset <- dataSubset[grep("/01/",dataSubset[,1]),] 
    
    #take the january average
    januaryAverage <- monthlyOrNotAdjuster*colMeans(januarySubset[,-1], na.rm = TRUE)
    
    
    
    julySubset <- dataSubset[grep("/07/",dataSubset[,1]),] 
    
    #take the july average
    julyAverage <- monthlyOrNotAdjuster*colMeans(julySubset[,-1], na.rm = TRUE)
    
    
    
    Results <- list("counts" =counts, "average" = averageAdjusted, "januaryAverage" = januaryAverage, "julyAverage" = julyAverage )    
  
  } else {
  
    Results <- list("counts" =counts, "average" = averageAdjusted)
  }
  
  return(Results)
}


reduceTable <- function(CalculatedTable, days1972Tolerance) {
  #the maximum number of days from 1972 thought to last irrigation year
  maxDays <- max(CalculatedTable[,2])
  
  minimumLimit <- maxDays - days1972Tolerance
  
  #find where the stations are within the limit
  positionStationsSelected <- which(CalculatedTable[,2] >= minimumLimit)
  
  ReducedTable <- CalculatedTable[positionStationsSelected,]
  
  
  return (ReducedTable)
  
}
  
printTable <-function(reducedTable, baseDirectory, summaryNameChosen){
  
  printFilePath <- file.path(baseDirectory, summaryNameChosen, fsep = "\\")
    
  write.csv(reducedTable,printFilePath, na = "", row.names = FALSE)
  

    
  
}


ExtendedSpreadData <- convertAllDatesToExcelFormat(ExtendedSpreadData)
  
#call function to calculate table
calculatedTable <- calculateAndOrganise(ExtendedSpreadData, dataType, startIrrYearRange)

#reduce the table
reducedTable <- reduceTable(calculatedTable, days1972Tolerance)

#print it
printTable(reducedTable, baseDirectory, summaryNameChosen)



