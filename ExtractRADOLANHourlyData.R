#init-----
rm(list=ls()) #clear workspace
graphics.off() #turn off any open graphics
options(
  warnPartialMatchDollar = T, #warn if incomplete variable names match existing variables
  dplyr.summarise.inform=F #Disable unnecessary info messages
)

library(raster) #for handling RADOLAN raster files
library(httr) #for extracting download URLs from DWD CDC websites
#for data handling:
library(stringr)
library(tidyr) 
library(dplyr)


#Set working directory
WorkDir <- "/path/to/WorkDir"

#Set proxy
#Set to empty string ("") if no proxy is required
Sys.setenv(
  http_proxy = "",
  https_proxy = ""
)
#General format:
#user:password@proxy:port
#https://stackoverflow.com/questions/6467277/proxy-setting-for-r
# Sys.setenv(
#   http_proxy = "proxy:8080",
#   https_proxy = "proxy:8080"
# )



#
# No changes required below this point -----------------------
#
StartTime <- Sys.time()

#Prepare I/O------
InDir <- file.path(WorkDir,"Input")
OutDir <- file.path(WorkDir,"Output")
dir.create(OutDir, showWarnings = F)

#Source helping functions-------
HelpFuns <- list.files(
  path = file.path(WorkDir,"HelpingFunctions"),
  pattern = ".R",
  full.names = T
)
sapply(
  X = HelpFuns,
  FUN = source
)

#Load target locations and time windows--------
TargetLocationsAndTimeSpans <- read.table(
  file = file.path(InDir,"TargetLocationsAndTimeSpans.csv"),
  sep = ";",
  header = T
)

#Download and extract the data-----------
#Data is incrementally written to a file in OutDir
GetRADOLANData(
  TargetLocationsAndTimeSpans = TargetLocationsAndTimeSpans,
  OutDir = OutDir,
  PrecipPrecision = 2
)



#Finish------
EndTime <- Sys.time()
TimeElapsed <- difftime(
  time1 = EndTime,
  time2 = StartTime,
  units = "hours"
)
TimeElapsed <- round(as.numeric(TimeElapsed),2)
print(paste("Start time:",StartTime))
print(paste("End time:",EndTime))
print(paste("Time elapsed:",TimeElapsed,"hours"))
AverageDurationPerInputRow <- round(TimeElapsed / nrow(TargetLocationsAndTimeSpans),4)
print(paste("Average duration per row in input file:",AverageDurationPerInputRow,"hours"))

