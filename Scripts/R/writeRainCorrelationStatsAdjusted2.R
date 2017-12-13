##########################################################
#
#  Generate summary statistics of rainfall correlations
#
##########################################################
# C. Fraser April 2016
#modified by Jonathan Dixon Nov 2016
#produces counts and averages of correlation from for 2 or 3 periods


rm(list = ls())                                               # clear the memory

#where the values extended summary is that you produced using Table of Station Values.R:
#note that this summary needs to end with "Correlation extended summary.csv" and the ONLY file that ends
#like this.
baseDirectory <- "C:\\Users\\j.dixon\\Desktop\\VP updates\\extension\\Summaries\\"


#the final irrigation year you want assessed that is available eg. Irrigation year 2015 starts 1st June 2015 and
# ends 31st May 2016:
finalIrrYear <- 2015

#number of periods you want assessed (2 or 3):
numberOfPeriods <- 2

#start irrigation Year for the periods
startIrrYear1 <- 1960

startIrrYear2 <- 1972

#if numberOfPeriods is 3, the value for startIrrYear3 won't be used:
startIrrYear3 <- 1972



summaryNameChosen <- "_Correlation_Summary.csv"



fileList <- list.files(baseDirectory)

#find the first file that has " extended.csv" in it
extendSpreadName <-  fileList[which(grepl("Correlation extended summary.csv", fileList))][1]

extendSpreadsheetPath <- file.path(baseDirectory,extendSpreadName)

#Read data
test<-read.csv(extendSpreadsheetPath)



# Add on some more date info
#NOTE, haven't actually used this extra info
 test$Date<-as.Date(test$Date,"%Y-%m-%d")
# test$YearFac <-  factor(format(test$myDate, "%Y"))
# test$Year <- as.numeric(as.character(test$YearFac))
# test$Month <- format(test$myDate, "%b")    # abbreviated  months
# test$yrmon <-paste(test$Year,test$Month, sep="-")
 
#convert the values to numeric
 indx <- sapply(test, is.factor)
 test[indx] <- lapply(test[indx], function(x) as.numeric(as.character(x))) 


#Get statistics about the entire time period
test2<-summary(test)

# collect the periods
start1 <- paste0((startIrrYear1),"-06-01")
start2 <- paste0((startIrrYear2),"-06-01") 
finalDate <- paste0((finalIrrYear+1),"-05-31")


#format YYYY-MM-DD
ind1<-which(test$Date>=start1&test$Date<=finalDate)
ind2<-which(test$Date>=start2&test$Date<=finalDate)



#Calculate mean values adn nubmer of NA values for each time period
Mean_p1<-apply(test[ind1,-c(1)],2,mean,na.rm=T)
Count_p1<-apply(test[ind1,-c(1)],2,function(x) length(which(!is.na(x)==T)))
Mean_p2<-apply(test[ind2,-c(1)],2,mean,na.rm=T)
Count_p2<-apply(test[ind2,-c(1)],2,function(x) length(which(!is.na(x)==T)))


#round values to 3dp:
Mean_p1 <- round(Mean_p1,3)
Mean_p2 <- round(Mean_p2,3)


if (numberOfPeriods == 3){
  #generate the stats for the 3rd period as has been done for the first 2
  start3 <- paste0((startIrrYear3),"-06-01") 
  
  ind3<-which(test$Date>=start3&test$Date<=finalDate)
  
  Mean_p3<-apply(test[ind3,-c(1)],2,mean,na.rm=T)
  
  Count_p3<-apply(test[ind3,-c(1)],2,function(x) length(which(!is.na(x)==T)))
  
  Mean_p3 <- round(Mean_p3,3)
  
}


#Extract the site names()
agent<-sapply(names(test[ind1,-c(1)]),function(x) strsplit(x,split=".extended")[[1]][1])

#remove the x's:
agent <- substring(agent,2)

#construct names for data:
finalSubscript <- substring(finalIrrYear,3)
Count1n <- paste0("Count_irrYear", startIrrYear1,"_", finalSubscript)
Count2n <- paste0("Count_irrYear", startIrrYear2,"_", finalSubscript)
Ave1n <- paste0("Ave_irrYear", startIrrYear1,"_", finalSubscript)
Ave2n <- paste0("Ave_irrYear", startIrrYear2,"_", finalSubscript)



 

if (numberOfPeriods == 2){
  #glue everything together and save:
  OUTPUT<-data.frame(agent,Count_p1,Count_p2,Mean_p1,Mean_p2)
  
  TableNames<- c("AgentNo", Count1n, Count2n, Ave1n, Ave2n)
  
} else if (numberOfPeriods == 3){
  #extra names:
  Count3n <- paste0("Count_irrYear", startIrrYear3,"_", finalSubscript)
  Ave3n <- paste0("Ave_irrYear", startIrrYear3,"_", finalSubscript)

  OUTPUT<-data.frame(agent,Count_p1,Count_p2, Count_p3, Mean_p1,Mean_p2, Mean_p3)
  
  TableNames<- c("AgentNo", Count1n, Count2n, Count3n, Ave1n, Ave2n, Ave3n)
  
}

names(OUTPUT) <- TableNames


printFilePath <- file.path(baseDirectory, summaryNameChosen, fsep = "\\")

write.csv(OUTPUT,printFilePath, na = "", row.names = FALSE)

