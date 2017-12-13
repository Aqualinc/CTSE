############################################
# Jonathan Dixon  


  rm(list=ls()) # clear memory
  
  #Specify the directory that holds the files that need to be reformatted
  FromDirectory <- "C:\\Users\\j.dixon\\Desktop\\9am obs corrected files\\Raw"
  
  #specify the directory to write the formatted files to
  ToDirectory <- "C:\\Users\\j.dixon\\Desktop\\9am obs corrected files\\VP\\VP converted"
  
  #directory if non-existent data
  FaultyDirectory <- "C:\\Users\\j.dixon\\Desktop\\9am obs corrected files\\9am T\\VP converted faulty"


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
        
        #
        
        #remove unneeded columns
        dataReduced <-data[, -c(1, 2, 4,5,6,7,9)]
        
        
        
      
        names(dataReduced)<- c("day","Tdew" )
        #change to character
        dataReduced[,2] <- as.character(dataReduced[,2])
        

        
        #get rid of rows with "-" in it:
        if (length(which(dataReduced$Tdew=="-"))>0) {
          dataReduced<-dataReduced[-which(dataReduced$Tdew=="-"),]
        }
        
        dataReduced$Tdew<-as.numeric(dataReduced$Tdew)
        #equation to get vp coming up:
        Td <-dataReduced$Tdew
        vp <- 0.6108*exp(17.27*Td/(Td+237.3))*10
       
         #add vp to the reduced data:
        dataReduced[,3] <- vp
        
        #change the name:
#         fileStripped <- strsplit(file, ".csv")
#         fileVP <-paste0(fileStripped,"vp.csv")  
        
        outFileName <- file.path(ToDirectory,file,fsep = "\\")

        ColumnsOfInterest <- c(1,3)
        
        write.table(dataReduced[,ColumnsOfInterest],outFileName,row.names=FALSE,col.names=FALSE,sep=",")
      } else {
        #write an empty file
        outFileName <- file.path(FaultyDirectory,file,fsep = "\\")
        file.create(outFileName)

      }
    } else {
      outFileName <- file.path(FaultyDirectory,file,fsep = "\\")
      file.create(outFileName)
    }
    
  }
  