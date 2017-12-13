##########################################################
#
#  Generate summary statistics of rainfall correlations
#
##########################################################
# C. Fraser April 2016

rm(list = ls())                                               # clear the memory

#Read data
test<-read.csv("Q:/Temp/Peter/Rain724824_extended_Correlation.csv")

#Write out first few rows for inspection in excel
write.csv(test[1:5,],"Q:/Temp/Peter/Rain724824_extended_CorrelationHEAD.csv")

# Add on some more date info
#NOTE, haven't actually used this extra info
test$Date<-as.Date(test$Date,"%Y-%m-%d")
test$YearFac <-  factor(format(test$myDate, "%Y"))
test$Year <- as.numeric(as.character(test$YearFac))
test$Month <- format(test$myDate, "%b")    # abbreviated  months
test$yrmon <-paste(test$Year,test$Month, sep="-")

#Get statistics about the entire time period
test2<-summary(test)

#Condsider date ranges: 1972-2015, 1960-2015 and 1915-2015
ind1<-which(test$Date>="1972-1-1"&test$Date<"2016-1-1")
ind2<-which(test$Date>="1960-1-1"&test$Date<"2016-1-1")
ind3<-which(test$Date>="1915-1-1"&test$Date<"2016-1-1")


#Calculate mean values adn nubmer of NA values for each time period
Mean_p1<-apply(test[ind1,-c(1,2893:2897)],2,mean,na.rm=T))
NA_p1<-apply(test[ind1,-c(1,2893:2897)],2,function(x) length(which(is.na(x)==T)))
Mean_p2<-apply(test[ind2,-c(1,2893:2897)],2,mean,na.rm=T)
NA_p2<-apply(test[ind2,-c(1,2893:2897)],2,function(x) length(which(is.na(x)==T)))
Mean_p3<-apply(test[ind3,-c(1,2893:2897)],2,mean,na.rm=T)
NA_p3<-apply(test[ind3,-c(1,2893:2897)],2,function(x) length(which(is.na(x)==T)))

#Extract teh site names()
sitenames<-sapply(names(test[ind1,-c(1,2893:2897)]),function(x) strsplit(x,split=".extended")[[1]][1])

#glue everything together and save
OUTPUT<-data.frame(SiteNames=sitenames,Mean_1972to2015=Mean_p1,nNA_1972to2015=NA_p1,Mean_1960to2015=Mean_p2,nNA_1960to2015=NA_p2,Mean_1915to2015=Mean_p3,nNA_1915to2015=NA_p3)           
write.csv(OUTPUT,"Q:/Temp/Peter/Rain724824_extended_CorrelationSUMMARY_2.csv")                 
