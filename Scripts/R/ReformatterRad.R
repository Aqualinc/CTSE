
  
  #Specify the directory that holds the files that need to be reformatted
  FromDirectory <- "C:\\Users\\j.dixon\\Desktop\\Climate Data\\Raw Cliflo Data\\Radiation stations NZ"
  
  #specify the directory to write the formatted files to
  ToDirectory <- "C:\\Users\\j.dixon\\Desktop\\Climate Data\\Converted Cliflo Data\\Radiation stations"
  
  #directory if non-existent data
  FaultyDirectory <- "C:\\Users\\j.dixon\\Desktop\\Climate Data\\Converted Cliflo Data\\Radiation stations faulty"
  
  #get the list of files that need reformatting
  fileList <- list.files(FromDirectory)
  
  for (file in fileList){
    fileOfInterest <- file.path(FromDirectory,file,fsep = "\\")
    #if the file is not empty
    if (length(readLines(fileOfInterest)) > 0){
      #get the data from it
      data  <-  read.csv(fileOfInterest,header=FALSE,skip=9,nrow=length(readLines(fileOfInterest))-17)
	        # remove NA values:
      data[is.na(data)] <- ""
      if (is.numeric(data[1,1])){
        #then write the file
        outFileName <- file.path(ToDirectory,file,fsep = "\\")
        ColumnsOfInterest <- c(3,5)
        write.table(data[,ColumnsOfInterest],outFileName,row.names=FALSE,col.names=FALSE,sep=",")
      }
      else {
        #write an empty file
        outFileName <- file.path(FaultyDirectory,file,fsep = "\\")
        file.create(outFileName)

      }
    } 
    else {
      outFileName <- file.path(FaultyDirectory,file,fsep = "\\")
      file.create(outFileName)
    }
    
  }
  