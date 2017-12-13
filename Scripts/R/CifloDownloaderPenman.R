# CLiflo downloader
#This script is written to automatically download data from NIWA's Cliflo website using the IMACRO on Firefox given the Agent number of
# the site of interest and the parameter type.

#********************************************
# Function to download the Cliflo Data
#
# This uses an external windows batch file
# which opens the Firefox web browser, executes
# an iMacros macro to get the data, then closes
# firefox.
# Very convoluted, but it works.
#********************************************

DownloadClifloData<-function(BaseDirectory="C:\\Users\\pbrow_000\\Desktop\\downloadingfiles\\",Agentfile="agents.csv",parameter="rainfall")
  #  DownloadClifloData<-function(BaseDirectory="\\\\some\\directory\\where\\the\\automation\\stuff\\is")
{
  defaultimacro<-readLines(paste0(BaseDirectory,"ClifloDownloaderPenman.iim"),-1)
  
  agents <- unlist(read.csv(paste0(BaseDirectory,Agentfile)))
    
  #Use the following line to find the Firefox path if you don't know what it is
  #FirefoxPath <- shell("where /R c:\\ firefox", intern=TRUE)
  
  FirefoxPath <- "c:\\Program Files (x86)\\Mozilla Firefox\\firefox.exe"
  FirefoxCommand1 <- paste0('start "" "',FirefoxPath, '"')
  shell(FirefoxCommand1)
  Sys.sleep(5)  #this is to ensure Firefox has completed opening before starting the next step
  #FirefoxCommand2 <- paste0('start /B /wait "" "',FirefoxPath, '" imacros://run/?m=NewClifloDownloader.iim')
  #shell(FirefoxCommand2)
  #Sys.sleep(10)  #this is to ensure the downloading has ocurred before processing it further
  
  #shell(start "" "%FirefoxPath%" imacros://run/?m=ClifloDownloader.iim)
  for (AgentNo in agents) {
    AgentLine <- paste0("TAG POS=1 TYPE=INPUT:TEXT FORM=NAME:gonzo ATTR=NAME:agents CONTENT=",AgentNo)
    OutputLine <- paste0("SAVEAS TYPE=TXT FOLDER=* FILE=",AgentNo,".csv")
    
    #the following 2 lines may need changing depending where the agent number is:
    
    defaultimacro[14] <- AgentLine
    defaultimacro[33] <- OutputLine
    writeLines(defaultimacro,paste0(BaseDirectory,"NewClifloDownloader.iim"))
    CopyShellText <- paste0('copy /Y "',BaseDirectory,'NewClifloDownloader.iim" C:\\Users\\pbrow_000\\Documents\\iMacros\\Macros\\NewClifloDownloader.iim')
    shell(CopyShellText)
    
    FirefoxCommand2 <- paste0('start /B /wait "" "',FirefoxPath, '" imacros://run/?m=NewClifloDownloader.iim')
    shell(FirefoxCommand2)
    Sys.sleep(10)  #this is to ensure the downloading has ocurred before proceeding further
   
  }
  shell('taskkill /IM Firefox.exe /f')

}

DownloadClifloData()