#Jonathan Dixon
#Records CSV files in a folder and writes a list of them in a single CSV file
#Specify the directory that holds the files that need to be reformatted
FromDirectory <- "C:\\Users\\j.dixon\\Desktop\\PET\\extension process\\PET update extension files\\PET extended with Tier Nov 2016"

#get the list of files that need reformatting
fileList <- list.files(FromDirectory)

fileListStripped <- strsplit(fileList, ".csv")

#convert from list
readyFiles <-unlist(fileListStripped)

file <- "list.csv"

#sets location:

outFileName <- file.path(FromDirectory,file,fsep = "\\")

write.table(readyFiles,outFileName,row.names=FALSE,col.names=FALSE,sep=",")

