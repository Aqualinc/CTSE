############################################
# Jonathan Dixon
#extend site data, based on correlations
########################################

############################
#set things up
rm(list=ls()) # clear memory
library(plyr)
library(hydroTSM)
library(caTools)
library(chron)
library(zoo)
library(stringi)
library(sp)

######################################################

#USERS AREA

# Extended data using Peter's "Climate timeseries tools(v2011-12-05)" can be added to the station folder, 
#this R program won't create an extended series for files with "out" in it, and is unlikely to create
#a successful one for any csv that has anything but the agent number in its title

#variables for user to alter:

# DATATYPE: have 1 for PET, rain, wind or vapour pressure
# have 2 for Temperature data
# have 3 for Radn data

DataType <- 3


#the directory where you have the converted files you want to perform the extension with: (your converted files and
#potentially Tier 1 stations for data types other than rain)
StationDirectory <-   "C:\\Users\\pbrow_000\\Desktop\\Rad updating\\Extension\\Updated and tier 1"
#obtain site(TS1) data: the default for this is the same folder but can be changed
SiteDirectory <- StationDirectory

WriteToDirectory <- "C:\\Users\\pbrow_000\\Desktop\\Rad updating\\Extension\\Radn extended with Tier Nov 2016"


#The following folder is where csv of stations information  kept- will only read first file!
#the agent numbers should be in the first column, the NZTM easting in the 11th column, the northing in the 12th
#will read from the 2nd row (ie assumes a header)
StationInfoDirectory <- "C:\\Users\\pbrow_000\\Desktop\\Rad updating\\Extension\\Station Info"

#running mean window size:
movingWindowSize <- 14

#start and finish dates of the file to be created:
#date format: "YYYY/MM/DD"
startDate <- "1960/1/1"  

finishDate <- "2016/11/22"

#select stations within radius limit of:
kmRadiusLimit <- 200



####################################
#Start of functions from correlationTable.R
datesInEachmonth <- function(MatchingDataDates) {
  
  #return the month of each piece of data:
  monthsOfData <- format(index(MatchingDataDates), "%m")
  
  monthstring <- c("01","02","03", "04", "05", "06", "07", "08", "09", "10", "11","12")
  
  monthTotals <- as.numeric()
  
  for (j in 1:12){
    
    #for each month find the number of dates with the relevant monthstring value:
    monthTotals[j] <- length(which(monthsOfData == monthstring[j]))     
    
  }
  
  
  return(monthTotals)
}



correlationTable <- function(SiteDirectory,siteFile, movingWindow, CloseEnoughStations) {
  
  

  #obtaining TS1 data
  

  
  site<- file.path(SiteDirectory,siteFile,fsep = "\\")
  
  siteData  <-  read.csv(site,header=FALSE)
  

  #setting up:
  
  stationList <- CloseEnoughStations
  
  #empty data frame:
  correlationValue <- data.frame(matrix(NA,nrow= length(stationList), ncol = 2))
  names(correlationValue) <- c("name", "corr")
  
  #sets position of data:
  stationPos <- 1
  

  #for loop for finding the correlation of matching dates between TS1 and TS2
  
  for (file in stationList){
    #station = TS2
    station <- file.path(StationDirectory,file,fsep = "\\")
    
    #if not an empty file:
    if (file.size(station) > 0){
      stationData <- read.csv(station,header=FALSE)
      
      MatchingData<-data.frame(Mydates=intersect(siteData$V1,stationData$V1))
      
      
      #convert to zoo format to work with the dates
      MatchingData <- zoo(MatchingData,order.by = as.Date(as.character(MatchingData[,"Mydates"]),format="%Y%m%d"))
      
      #Use funtion to find number of dates in each month
      
      
      if (nrow(MatchingData)>1) {  
        
        #the dates that match between TS1 and TS2:
        newdates <-intersect(siteData$V1,stationData$V1)
        
        #data frame of matching dates and values:
        MatchingData$siteData<-siteData[match(newdates,siteData$V1),2]
        MatchingData$stationData<-stationData[match(newdates,stationData$V1),2]
        
        
        #uncomment these if you want a plot
        #plotTitle <- paste0(siteFile, " vs ", file)
        
        #plot(MatchingData$siteData,MatchingData$stationData, main = plotTitle, xlab = siteFile, ylab =file)
        
        
        
        
        
        #find the moving average with a window of 4:
        siteMovingAverage <- runmean(MatchingData$siteData,movingWindow)
        
        stationMovingAverage <- runmean(MatchingData$stationData, movingWindow)
        #adding correlation value to table:
        correlationValue$corr[stationPos] <- cor(siteMovingAverage,stationMovingAverage)
      } else { 
        correlationValue$corr[stationPos] <- 0
        
      }
      
      datesPerMonth <- datesInEachmonth(MatchingData)
      
      #if there are more than 30 dates in each month that match (can be changed):
      if (all(datesPerMonth >30, na.rm = FALSE)){
        correlationValue$EnoughData[stationPos] <- TRUE
        
      } else{
        correlationValue$EnoughData[stationPos] <- FALSE
        
      }
    
    } else{
      correlationValue$corr[stationPos] <- 0
      correlationValue$EnoughData[stationPos] <- FALSE
      
    }
    
    #build the correlation table:  
    correlationValue$name[stationPos] <- c(file)
    
    
    stationPos <- stationPos +1  
    
  }
  
  
  
  #call function to reorder table into parts
  eligibleAndRejectedTables <- ReorderTable(correlationValue, siteFile)
  
  return(eligibleAndRejectedTables)
}


ReorderTable <- function(CorrelationValue, SiteFile){
  
  #order rows by correlation value
  
  CorrelationValue <- CorrelationValue[with(CorrelationValue, order(-corr)), ]
  
  #those for which enough data is true:
  StationsToUse <- CorrelationValue[which(CorrelationValue$EnoughData == T),]
  
  stationsNotUsed <- CorrelationValue[which(CorrelationValue$EnoughData == F),] 
  
  #round correlations to 3 dp:
  StationsToUse$corr<- round(StationsToUse$corr,3)
  stationsNotUsed$corr<- round(stationsNotUsed$corr,3)
  
  
    #those with "out" in them:
  outStationsToUse <- StationsToUse[which(grepl("out", StationsToUse$name)),]
  
  if (nrow(outStationsToUse)>0) { #if there are any out files:
    #remove those with "out" from the original list: 
    StationsToUse <- StationsToUse[-which(grepl("out", StationsToUse$name)),]
    

    #add the "out"stations on the end so they are lower priority
    StationsToUse <- rbind(StationsToUse,outStationsToUse)
  }
  
  
  #name of extended, if exists:
  
  siteNameReduced <- strsplit(SiteFile, ".csv")
  siteNameExtended <- paste0(siteNameReduced, " out")
  
  #if there is an extended station
  if (any(grepl(siteNameExtended,StationsToUse$name), na.rm=FALSE)) {
    StationsToUse$corr[which(grepl(siteNameExtended,StationsToUse$name))] <- "N.A."
  }
  
  Results <- list(StationsToUse,stationsNotUsed)
  
  return(Results)
}


#end of functions from correlation table

###############################################################

orderedAverages <- function (unorderedMonthlyAverages){
  #returns ordered averages from the monthlyfunction
  
  MonthString<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct" ,"Nov", "Dec")
  
  orderedMatchingSiteMonthlyAverages <-numeric()
  
  # for loops finds the average the matches each "monthstring" value and orders them accordingly
  for (k in 1:12) {
    kthMonthPosition = which(index(unorderedMonthlyAverages) == MonthString[k])
  
    orderedMatchingSiteMonthlyAverages[k] = unorderedMonthlyAverages[kthMonthPosition]
  }  
  
  return(orderedMatchingSiteMonthlyAverages)
}



returnmonthlyAdjusters <- function(siteDataFrame,stationDataFrame,dataType) {
  #returns monthly ratios of TS1(site)/TS2(station) average monthly values

  newdates <-intersect(siteDataFrame$V1,stationDataFrame$V1)         #Find the dates that match
  MatchingData<-data.frame(Mydates=newdates)                         #Create a new data frame and give it the dates that are available in both the site of interest and the correlating site
    
  MatchingData$siteData<-siteDataFrame[match(newdates,siteDataFrame$V1),2]          #Add a column of the data from the site of interest (TS1 or time series 1)
  MatchingData$stationData<-stationDataFrame[match(newdates,stationDataFrame$V1),2] #Add a column of the data from the correlating site (TS2 or timeseries 2)
  
  #convert to zoo format to work with the dates
  MatchingData <- zoo(MatchingData,order.by = as.Date(as.character(MatchingData[,"Mydates"]),format="%Y%m%d"))
  
  #obtain the TS1(site) and TS2 (station) monthly averages in order:
  matchingStationMonthlyAverages <- monthlyfunction(MatchingData[,c("stationData")], FUN =mean) #get the month averages of the data from the site of interest (TS1)
  stationOrderedAverages <- orderedAverages(matchingStationMonthlyAverages)                     #put them in order from Jan - Dec
    
  matchingSiteMonthlyAverages <- monthlyfunction(MatchingData[,c("siteData")], FUN =mean)       #get the month averages of the data from the correlating site (TS2)
  siteOrderedAverages <- orderedAverages(matchingSiteMonthlyAverages)                           #put them in order from Jan - Dec
  
  if (dataType == 1 || dataType == 3) {  #eg rain or radiation
  #divide one by the other to get monthly averages
    monthlyAdjusters<- as.numeric(siteOrderedAverages) / as.numeric(stationOrderedAverages)
    
  } else if (dataType ==2)  {  #eg if temperature data
    
    # subtract site(TS1) average from station(TS2) average
    monthlyAdjusters<- as.numeric(siteOrderedAverages) - as.numeric(stationOrderedAverages)
  }
  
  return(monthlyAdjusters)
}



returnConvertedStationData <- function(unconvertedStationData,monthlyAdjusters, DataType) {
  
  MonthString<- c(1:12)
  
  
  #split up the dates so month is accessible
  splitDates = unlist(strsplit(as.character(index(unconvertedStationData)), split='-', fixed=TRUE))
  
  
  monthList = numeric()
  #extract the month for each row
  for (h in 1:length(unconvertedStationData)){
    monthList[h] = splitDates[3*(h-1)+2]
  }
  
  monthList <- as.numeric(monthList)
  
  #add the relevant month column to the data:
  unconvertedStationData <- merge(unconvertedStationData,monthList)
  
  
  if (DataType == 1 || DataType == 3) {
    #divide one by the other to get monthly averages
    unconvertedStationData$converted <- unconvertedStationData[,1]*monthlyAdjusters[unconvertedStationData[,2]]
    
  } else if (DataType ==2)  {  #eg if temperature data
    
    # subtract site(TS1) average from station(TS2) average
    unconvertedStationData$converted <- unconvertedStationData[,1] + monthlyAdjusters[unconvertedStationData[,2]]
  }
  
  
  
  
  
  
  
  
  
  
  
  convertedStationData <- unconvertedStationData
  
  return(convertedStationData)
  
}
  
##################################################################  
extendSiteData <- function(StationDirectory,SiteDirectory, siteData, corrtable, startingDate, finishdate, datatype) {
  
  totalTable <- data.frame(Date= seq(as.Date(startingDate), as.Date(finishdate), by = "day"), Value = NA, Origin =NA, Correlation =NA)   #set up total table. This is an empty four column dataframe with the first column dates from the "startingDate" to the "finishDate" 
  nonNaMatchesTT <- match(index(siteData), totalTable$Date)[!is.na(match(index(siteData), totalTable$Date))]      #Find the positions in "siteData" that have data:
  nonNaMatchesSite <- match(totalTable$Date, index(siteData))[!is.na(match(totalTable$Date, index(siteData)))]    #Find the positions of the dates in the "totalTable" for which data exists in siteData (TS1, or Time Series 1):
  totalTable$Value[nonNaMatchesTT] <- siteData[nonNaMatchesSite] #add the good data into the "totalTable"
  totalTable$Origin[nonNaMatchesTT] <- "original"                #And add the comment "original" to the "Origin" column
  totalTable$Correlation[nonNaMatchesTT] <- 1                    #And the number "1" to the "Correlation" column
 
  #For the dates with missing data find the most correlated site and use its data to fill the gaps
  if (nrow(corrtable) >0) {                #Check that there is at least one other site that is correlated
  
    for (n in 1:nrow(corrtable)){                       #Work through each of the correlated site until finding one that has data on the date of interest
	#Note that it might be quicker to use a while loop based on whether there are any missing dates or not. Something for the future.
      stationFile <- corrtable[n,1]                     #Select the first (highest correlated) site
      #read the correlated site data in as a zoo (i.e. timeseries) object  
      stationData  <-  read.zoo(file.path(StationDirectory,stationFile),index.column=1,sep=",",format="%Y%m%d",header=FALSE,regular=FALSE)
      MissingDatesIndex <- which(is.na(totalTable$Value) == TRUE)     #Find the index of the dates that are missing
      missingDates <- totalTable[MissingDatesIndex,"Date"]            #Find the dates that are missing
      stationDates <- index(stationData)							  #get the available dates from the correlated site
      matchingResults<- match(missingDates, stationDates)             #Find which of the missing dates are available in the correlated site's data
      noNaMatchingResults <- which(is.na(matchingResults) ==F)        #Find the indices of the dates that are available
  
      ############################################################    
      #constructing monthly ratios vector
      #Read in the site data (TS1 or time series 1) and the correlated site data (TS2 or time series 2)
	  #Note that "siteFile" has not been set as a function parameter, so it is assumed it already exists. This is even though the actual site data is a parameter!!
	  #Note also that the correlated site's data has already been read in above. Bit of duplication here!!
	  #It would be good to simply use the site data, rather than read it in all over again. Something for the future.
      siteDataDf    <- read.csv(file.path(SiteDirectory,siteFile,fsep = "\\"),header=FALSE)        #read in the site data
      stationDataDf <- read.csv(file.path(StationDirectory,stationFile,fsep = "\\"),header=FALSE)  #read in the coorelated site's data
      monthlyAdjuster <- returnmonthlyAdjusters(siteDataDf,stationDataDf, datatype)                #Find the parameters to correct the correlated site to the site of interest
      #########################################################################################
       
      usefulStationData <- stationData[matchingResults[noNaMatchingResults]]                       #Find the rows from the correlated site that will be used to fill the gaps
      usefulStationData <- returnConvertedStationData(usefulStationData,monthlyAdjuster, datatype) #Apply the conversion formula
      totalTable$Value[MissingDatesIndex[noNaMatchingResults]] <- usefulStationData$converted      #And put the converted values into the "totalTable"
      totalTable$Origin[MissingDatesIndex[noNaMatchingResults]] <- stationFile                     # add origin: station name
      totalTable$Correlation[MissingDatesIndex[noNaMatchingResults]] <- stationFile <- corrtable[n,2]   #And add in the correlation coefficient
      }   #end of the loop to go through all the correlated sites

  }
     return(totalTable)    
}


printData <- function(siteFile, totaltable, writeToDirectory, corrtable, notUsedCorrtable, DataType) {
  #replace meaningless 0 correlation values
  notUsedCorrtable$corr[which(notUsedCorrtable$corr == 0)] <- "N.A."
  
  totaltable$Date <- as.character(totaltable$Date)
  
  nonNAvalues <- totaltable$Value[which(!is.na(totaltable$Value))]
  
  if(DataType == 3){#eg if radiation
    numbDP <- 2
  }else{
    numbDP <- 1
  }
  
  roundedNonNavalues <- round(nonNAvalues,numbDP)
  
  totaltable$Value[which(!is.na(totaltable$Value))] <- roundedNonNavalues
  
  
  #set up data frame for printing
  finalPrint <- data.frame(matrix(NA, nrow = nrow(totaltable), ncol = 11))
  

  #add total table data
  finalPrint[1:nrow(totaltable),1:ncol(totaltable)] <- totaltable[,,]
   
  corrtablePosition <- ncol(totaltable) +3
  
  if (nrow(corrtable) > 0){ 
    #add in correlation table:
    finalPrint[1:nrow(corrtable),corrtablePosition:(corrtablePosition+1)] <-corrtable[,1:2]
  
  }
   
  rejectedPosition <- ncol(totaltable) + 2 + ncol(corrtable) + 1
  
  
  if (nrow(notUsedCorrtable) > 0){  
  #add in rejected correlation table: 
  finalPrint[1:nrow(notUsedCorrtable),rejectedPosition:(rejectedPosition+1)] <- notUsedCorrtable[,1:2]
  
  }
  
  names(finalPrint)<- c("Date","Value","Origin","Correlation","","","Eligible Stations","Correlation","","Rejected","Correlation")
  
  #remove NAs from the data frame:
  finalPrint[is.na(finalPrint)] <- ""
  
  #remove ".csv" from file name
  siteNameStripped <- strsplit(siteFile, ".csv")[[1]]
  
  finalName <- paste0(siteNameStripped, " extended.csv")
  
  outFileName <- file.path(writeToDirectory,finalName,fsep = "\\") 
  
  #write to the CSV#  
  write.table(finalPrint,outFileName,row.names=FALSE,col.names=TRUE,sep=",")
  
  
  
  
  
}
  

chooseTS2Stations <- function(StationInfoDirectory, StationDirectory, SiteFile, kmRadiusLimit) {
  #select station files whose agent number is within the radius in km
  
  #there should only be one information file in StationInfoDirectory, it will only read the first one:
  infoList <-list.files(StationInfoDirectory)
  
  
  infoFile <- file.path(StationInfoDirectory,infoList[1],fsep = "\\")
  
  csvData  <-  read.csv(infoFile,header=TRUE)
  
  #finds the respective information from the 1st, 11th and 12th columns of data
  csvInfo <-data.frame(AgentNumber = csvData[,1], nztmEasting = csvData[,11], nztmNorthing = csvData[,12])
  
  
  
  siteAgentNumber <- strsplit(SiteFile, ".csv")[[1]]
  
  siteInfo <- csvInfo[which(csvInfo$AgentNumber == siteAgentNumber),]
  
  stationList <- list.files(StationDirectory)
  
  #remove the actual TS1 site from station list
  stationList <- stationList[!stationList == SiteFile]
  
  stationListCsvStripped <- character()
  stationListCsvStripped <- strsplit(stationList, ".csv")
  #change back to character string
  stationListCsvStripped <- as.character(stationListCsvStripped)
  
  #setting up for the loop
  
  stationsCloseEnough <- character()
  closeEnoughPosition <-1
  
  for (stationName in stationListCsvStripped){
    
    #in case there are some extended file ie "3925 out" or "4843 extended", only keep before the first space:
    stationAgentNumber <- strsplit(stationName, " ")[[1]][1]
    
    #find the TS2 station information
    stationInfo <- csvInfo[which(csvInfo$AgentNumber == stationAgentNumber),]
    
    eastingDistance <- stationInfo$nztmEasting - siteInfo$nztmEasting
    
    northingDistance <- stationInfo$nztmNorthing - siteInfo$nztmNorthing
    
    distanceBetween <- sqrt(eastingDistance^2 + northingDistance^2)
    
    kmDistanceBetween <- distanceBetween/1000
    
    if (kmDistanceBetween <= kmRadiusLimit) {
      #add the name to the list
      stationsCloseEnough[closeEnoughPosition] <- stationName
      closeEnoughPosition <- closeEnoughPosition + 1
    }
    
    
    
  }  
  #add .csv on the end of each one
  stationsCloseEnough <- paste0(stationsCloseEnough, ".csv")

  return (stationsCloseEnough)
  
}







#############################################################


siteList <-list.files(SiteDirectory)
###############################################

#########################################################
for (siteFile in siteList){
  #if the name of the file doesn't contain "out" in it
  if(length(grep("out",siteFile))==0 ) {
    
    siteFilePath <- file.path(SiteDirectory,siteFile)
    
    #if site data is not empty:
    if(file.size(siteFilePath)>0){
    
      siteData  <-  read.zoo(siteFilePath,index.column=1,sep=",",format="%Y%m%d",header=FALSE,regular=FALSE)
      #the window of the running average: 
      
      CloseEnoughStations <- chooseTS2Stations(StationInfoDirectory, StationDirectory, siteFile, kmRadiusLimit)
      
      
      if(!CloseEnoughStations[1] == ".csv") { #if there is any stations close enough:
        #construct correlation tables:
        tables <- correlationTable(SiteDirectory,siteFile, movingWindowSize, CloseEnoughStations) 
        
        #the first table is the correlation tables selected
        corrtable <- tables[[1]]
        
        notUsedCorrtable <- tables[[2]]
      } else {
        corrtable <- data.frame(matrix(NA,nrow= 0, ncol = 0))
        
        notUsedCorrtable <- data.frame(matrix(NA,nrow= 0, ncol = 0))
      }
      
      
      TotalTable <- extendSiteData(StationDirectory,SiteDirectory, siteData, corrtable, startDate, finishDate, DataType)
      
      #uncomment if you want it to print:
      printData(siteFile, TotalTable, WriteToDirectory, corrtable,notUsedCorrtable, DataType)
    }
  }
}


#############################################################

