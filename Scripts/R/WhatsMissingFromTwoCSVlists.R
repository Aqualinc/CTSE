#What's missing in different lists in two different csv's (only agent number list)
#Writes it to two separate CSV files

#enter the address of the actual csv file (not just the folder)
ListToMatchAcsv <- "C:\\Users\\j.dixon\\Desktop\\new appropriate list.csv"

ListToMatchBcsv <- "C:\\Users\\j.dixon\\Desktop\\current appropriate stations.csv"

#the address of the folder to write to:
WriteToDirectory <- "C:\\Users\\j.dixon\\Desktop\\"

#obtain the data:

DataA <- read.csv(ListToMatchAcsv,header =FALSE)

DataB <- read.csv(ListToMatchBcsv,header =FALSE)

#find those which give NA when searched for

missingFromA <- DataB[which(is.na(match(DataB[,], DataA[,] ))),]

missingFromB <- DataA[which(is.na(match(DataA[,], DataB[,] ))),]
  
 

file1 <- "MissingFromAlist.csv"

#sets location:

outFileName <- file.path(WriteToDirectory,file1,fsep = "\\")

write.table(missingFromA ,outFileName,row.names=FALSE,col.names=FALSE,sep=",")

file2 <- "MissingFromBlist.csv"
#sets location:

outFileName <- file.path(WriteToDirectory,file2,fsep = "\\")

write.table(missingFromB,outFileName,row.names=FALSE,col.names=FALSE,sep=",")


