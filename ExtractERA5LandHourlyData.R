#init-----
rm(list=ls())
graphics.off()
options(
  stringsAsFactors = F
)
library(tidyverse) #Data handling
library(lubridate) #Date-time handling
library(ecmwfr) #Actual interface to ERA5 data / ECMWF server
library(ncdf4) #for NetCDF handling

#Set parameters----

#Copernicus data storage (CDS) credentials
UID <- "1234"
APIKey <- "..."

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

#Define the number of decimal places for the extracted values
#(NetCDF files provide values with lots of decimal places)
ValueDecimalPrecision <- 2

#Download timeout
#This is the timeout to wait for the CDS server to provide
#the download file corresponding to a single row in the
#input CSV. If download does not start within this time span,
#the file is requested again from the CDS server (but the first
#request might still be pending in the download queue of the CDS
#server). This time span is a trade-off between avoiding multiple
#requests for the same file and waiting too long for a failed
#request (e.g. due to changed IP adress / network issues on the
#client site).
DownloadTimeout_s <- 60 * 60 * 6 #6 hours


#No changes required below this point ---------------------
StartTime <- Sys.time()

#Prepare I/O-----
InDir <- file.path(WorkDir,"Input")
OutDir <- file.path(WorkDir,"Output")
dir.create(OutDir,showWarnings = F)
TempFileName <- "ERA5TempFile.nc"
#Prepare output file for appending data
OutputFileName <- file.path(OutDir,paste0("ERA5_extracted_",format(StartTime,"%Y-%m-%d %H-%M-%S"),".csv"))

#Prepare coords for points to extract----
PointCoordsPath <- file.path(InDir,"PointCoords.csv")
if ( !file.exists(PointCoordsPath) ) {
  stop(paste("File not found:", PointCoordsPath))
}
PointCoords <- read.table(
  file = PointCoordsPath,
  header = T,
  sep = ";",
  dec = ".",
  stringsAsFactors = F
) %>%
  select(LocationLabel,Lat_EPSG4326,Lon_EPSG4326,DateStart,DateEnd,Parameter,Dataset) %>%
  mutate(
    LocationLabel = as.character(LocationLabel)
  )
nPointCoords <- nrow(PointCoords)
if ( nPointCoords == 0 ) {
  stop("No rows found in file PointCoords.csv.")
}
if ( any( is.na(PointCoords) ) ) {
  stop("PointCoords.csv must not contain NA.")
}
PointCoords <- PointCoords %>%
  mutate(
    DateStart = as.Date(
      x = DateStart,
      tryFormats = c("%Y-%m-%d","%d.%m.%Y")
    ),
    DateEnd = as.Date(
      x = DateEnd,
      tryFormats = c("%Y-%m-%d","%d.%m.%Y")
    )
  )
if ( any( is.na(PointCoords) ) ) {
  stop("Columns DateStart and DateEnd in PointCoords.csv must contain dates in format \"YYYY-MM-DD\"")
}


#Set CDS credentials-----
wf_set_key(
  user = UID,
  key = APIKey,
  service = "cds"
)


#Get data-----
#_Loop over input rows------
#in PointCoords.csv and download data
IsFirstSaveOperation <- T
for ( iRow in 1:nrow(PointCoords) ) {
  print(paste("Downloading data for input file row",iRow,"of",nrow(PointCoords)))
  
  #Get information for current row
  CurrentRowLabel <- PointCoords$LocationLabel[iRow]
  CurrentRowLat <- PointCoords$Lat_EPSG4326[iRow]
  CurrentRowLon <- PointCoords$Lon_EPSG4326[iRow]
  CurrentRowParameter <- PointCoords$Parameter[iRow]
  CurrentRowDateStart <- PointCoords$DateStart[iRow]
  CurrentRowDateEnd <- PointCoords$DateEnd[iRow]
  CurrentDataset <- PointCoords$Dataset[iRow]
  
  #Split by calendar years
  #To avoid large chunks of data to be downloaded (slow), a separate download sessions is used
  #per LocationLabel x Parameter x Calendar year
  FirstYear <- as.numeric(format(CurrentRowDateStart,"%Y"))
  FirstMonth <- as.numeric(format(CurrentRowDateStart,"%m"))
  LastYear <- as.numeric(format(CurrentRowDateEnd,"%Y"))
  LastMonth <- as.numeric(format(CurrentRowDateEnd,"%m"))
  Years <- FirstYear:LastYear
  
  #__Loop over years----
  
  for ( CurrentYear in Years  ) {
   
    if ( CurrentYear == FirstYear ) {
      MonthStart <- FirstMonth
    } else {
      MonthStart <- 1
    }
    if ( CurrentYear == LastYear ) {
      MonthEnd <- LastMonth
    } else {
      MonthEnd <- 12
    }    
  
    #___Prepare API request----
    #FEATURE:
    #Currently, the "area" parameter for the API call is set to the target location plus/minus
    #some very small buffer. This should results in exactly one grid cell being returned from
    #the Copernicus server. However, in rare occasions, the buffer might cross a grid cell
    #boundary, so data for two grid cells would be returned from the server (breaking the
    #script). This could be improved by identifying/checking if the correct grid cell
    #is returned/extracted.
    LatLonBuffer <- 0.0001
    LatFrom <- CurrentRowLat - LatLonBuffer
    LatTo <- CurrentRowLat + LatLonBuffer
    LonFrom <- CurrentRowLon - LatLonBuffer
    LonTo <- CurrentRowLon + LatLonBuffer
    Hours <- sprintf(fmt = "%02d:00",0:23)
    Months <- sprintf(fmt = "%02d",MonthStart:MonthEnd)
    Days <- sprintf(fmt = "%02d",1:31)
      
    APIRequest <- list(
      "dataset_short_name" = CurrentDataset,
      "product_type" = "reanalysis",
      "variable" = CurrentRowParameter,
      "year" = CurrentYear,
      "month" = Months,
      "day" = Days,
      "time" = Hours,
      #FIXME order of coords needs check
      #N/W/S/E
      "area" = paste0(LatTo,"/",LonFrom,"/",LatFrom,"/",LonTo),
      "format" = "netcdf",
      "target" = TempFileName
    )
    
    #___Clear temp file-----
    #Delete file from previous download if existing
    TempFilePath <- file.path(OutDir,TempFileName)
    if ( file.exists(TempFilePath) ) {
      file.remove(TempFilePath)
    }
    
    #Retry as long as file to to download is not found in folder
    iDownloadAttempt <- 0
    while ( !file.exists(TempFilePath) ) {
      iDownloadAttempt <- iDownloadAttempt + 1
      print(paste("...for (selected months in) year",CurrentYear,", download attempt",iDownloadAttempt))
    
      #___Download data-----
      #Catch errors in case of IP change or other network issues.
      tryCatch(
       expr =  {
         wf_request(
          user = UID,
          request  = APIRequest,
          transfer = TRUE,
          path = OutDir,
          #Maximum duration to wait until the download for single row of the input file starts
          time_out = DownloadTimeout_s
         )
       },
       error = function(cond) {
         message(paste("An error occurred when downloading the data for input row", iRow,"for year",CurrentYear,":"))
         message(cond)
         SleepTime_s <- 10
         message(paste("\nWaiting",SleepTime_s,"seconds before retrying..."))
         Sys.sleep(SleepTime_s)
       }
      )
      
    } #end of Retry as long as file to to download is not found in folder
    
    #___Extract data from NetCDF file----
    NetCDFFileHandle <- nc_open(TempFilePath)
    #Sanity-check NetCDF file
    CurrentVarName <- names(NetCDFFileHandle$var)
    if ( length(CurrentVarName) != 1 ) {
      stop("There should be exactly one variable per NetCDF file downloaded.")
    }
    if ( !(NetCDFFileHandle$var[[1]]$dim[[1]]$name == "longitude") ) {
      stop("Expected longitude as first dimension of NetCDF file.")
    }
    CurrentGridCellLon <- NetCDFFileHandle$var[[1]]$dim[[1]]$vals
    if ( length(CurrentGridCellLon) != 1 ) {
      stop("Data from more than one grid cell extracted for current point location.")
    }
    if ( !(NetCDFFileHandle$var[[1]]$dim[[2]]$name == "latitude") ) {
      stop("Expected latitude as second dimension of NetCDF file (lon).")
    }
    CurrentGridCellLat <- NetCDFFileHandle$var[[1]]$dim[[2]]$vals
    if ( length(CurrentGridCellLon) != 1 ) {
      stop("Data from more than one grid cell extracted for current point location (lat).")
    }
    if ( !(NetCDFFileHandle$var[[1]]$dim[[3]]$units == "hours since 1900-01-01 00:00:00.0") ) {
      stop("Expected time in hours since 1900-01-01 as third dimension of NetCDF file.")
    }
    #Extract time
    #In hours since 1900-01-01 00:00:00.0
    CurrentTimes <- NetCDFFileHandle$var[[1]]$dim[[3]]$vals
    #Extract parameter values
    CurrentValues <- ncvar_get(
      nc = NetCDFFileHandle,
      varid = CurrentVarName,
      verbose = F,
      raw_datavals = F
    )
    nc_close(NetCDFFileHandle)
    
    #___Save data----
    #Time comes in UTC
    #https://confluence.ecmwf.int/pages/viewpage.action?pageId=149325793
    DateTimeZero_UTC <- ymd_hms("1900-01-01 00:00:00")
    DateTime_UTC <- DateTimeZero_UTC + hours(CurrentTimes)
    CurrentOutput <- data.frame(
      TimeStamp_UTC = DateTime_UTC,
      Value = round(CurrentValues,ValueDecimalPrecision),
      LocationLabel = CurrentRowLabel,
      Dataset = CurrentDataset,
      Parameter = CurrentRowParameter
    ) %>%
      select(LocationLabel, TimeStamp_UTC, Parameter, Dataset, Value)
    write.table(
      x = CurrentOutput,
      file = OutputFileName,
      sep = ";",
      row.names = F,
      #Write header row only for the first var in first file.
      #Else, just append the data.
      append = ifelse(
        test = IsFirstSaveOperation,
        yes = F,
        no = T
      ),
      col.names = ifelse(
        test = IsFirstSaveOperation,
        yes = T,
        no = F
      )
    )
    IsFirstSaveOperation <- F
    
  } #end of loop over years
  
} #end of loop over rows in input table

#___Clear temp file if existing-----
TempFilePath <- file.path(OutDir,TempFileName)
if ( file.exists(TempFilePath) ) {
  file.remove(TempFilePath)
}

#Finish------
EndTime <- Sys.time()
TimeElapsed <- difftime(
  time1 = EndTime,
  time2 = StartTime,
  units = "hours"
)
TimeElapsed <- round(as.numeric(TimeElapsed),4)
print(paste("Start time:",StartTime))
print(paste("End time:",EndTime))
print(paste("Time elapsed:",TimeElapsed,"hours"))
AverageDurationPerInputRow <- round(TimeElapsed / nrow(PointCoords),4)
print(paste("Average duration per row in input file:",AverageDurationPerInputRow,"hours"))
