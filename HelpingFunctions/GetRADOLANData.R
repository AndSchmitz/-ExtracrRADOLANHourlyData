GetRADOLANData <- function(
  TargetLocationsAndTimeSpans,
  OutDir,
  PrecipPrecision
) {

  #Prepare output file for appending data
  OutputFileName <- file.path(OutDir,paste0("RADOLAN_extracted_",format(StartTime,"%Y-%m-%d %H-%M-%S"),".csv"))
  #Set flag indicating whether to write header to output table
  #(output incrementally is incrementally written)
  IsFirstSaveOperation <- T
  
  #Check input TargetLocationsDates-----
  
  #_Check for missing values-----
  #Drop lines completely NA
  TargetLocationsAndTimeSpans[TargetLocationsAndTimeSpans == ""] <- NA
  IndicesOfNARows <- which(apply(
    X = TargetLocationsAndTimeSpans,
    MARGIN = 1,
    FUN = function(x)all(is.na(x))
  ))
  if ( length(IndicesOfNARows) > 0 ) {
    TargetLocationsAndTimeSpans <- TargetLocationsAndTimeSpans[-IndicesOfNARows,]
  }
  if ( nrow(TargetLocationsAndTimeSpans) == 0 ) {
    stop("Error in function ExtractRADOLANData(): Table with target locations and time spans seems to be empty.")
  }
  #Check for missing values
  for ( iRow in 1:nrow(TargetLocationsAndTimeSpans) ) {
    for ( iCol in 1:ncol(TargetLocationsAndTimeSpans) ) {
      if ( is.na(TargetLocationsAndTimeSpans[iRow,iCol]) ) {
        stop(paste("Error in function ExtractRADOLANData(): Table with target locations and time spans contains empty cell in row",iRow+1,", column",iCol))
      }
    }
  }
  
  #_Check input for valid date format------
  #Determine TargetFormat and convert DateStart and DateEnd to date format
  DateFormat1 <- "%Y-%m-%d"
  DateFormat2 <- "%d.%m.%Y"
  for ( iRow in 1:nrow(TargetLocationsAndTimeSpans) ) {
    for ( CurrentCol in c("DateStart","DateEnd") ) {
      CurrentDateString <- TargetLocationsAndTimeSpans[iRow,CurrentCol]
      tmp <- as.Date(CurrentDateString, format = DateFormat1)
      if ( !is.na(tmp) ) {
        TargetFormat <- DateFormat1
      } else {
        tmp <- as.Date(CurrentDateString, format = DateFormat2)
        if ( is.na(tmp) ) {
          stop(paste("Input date in row",iRow,"is not a valid date (format):",CurrentDateString))
        } else {
          TargetFormat <- DateFormat2
        }
      }
    }
  }
  TargetLocationsAndTimeSpans <- TargetLocationsAndTimeSpans %>%
    mutate(
      DateStart = as.Date(DateStart, format = TargetFormat),
      DateEnd = as.Date(DateEnd, format = TargetFormat)
    )
  
  
  #Expand input table to hourly level-----
  #Expand input table to one row per LocationLabel x Date
  TargetLocationsDatesAndHours <- list()
  for ( iRow in 1:nrow(TargetLocationsAndTimeSpans) ) {
    CurrentInputRowData <- data.frame(
      LocationLabel = TargetLocationsAndTimeSpans$LocationLabel[iRow],
      LatEPSG4326 = TargetLocationsAndTimeSpans$LatEPSG4326[iRow],
      LonEPSG4326 = TargetLocationsAndTimeSpans$LonEPSG4326[iRow],
      Date = seq(TargetLocationsAndTimeSpans$DateStart[iRow], TargetLocationsAndTimeSpans$DateEnd[iRow], by = 1)
    )
    TargetLocationsDatesAndHours[[iRow]] <- CurrentInputRowData
  }
  TargetLocationsDatesAndHours <- do.call(rbind.data.frame, TargetLocationsDatesAndHours)
  #Expand input table to one row per LocationLabel x Date x Hour
  TargetLocationsDatesAndHours <- TargetLocationsDatesAndHours[rep(x = 1:nrow(TargetLocationsDatesAndHours), each = 24),]
  TargetLocationsDatesAndHours$Hour <- rep(0:23)
  
  
  #Get download URLs for each row-----
  print("Getting download URLs for required time spans...")
  UniqueDatesAndHours <- TargetLocationsDatesAndHours %>%
    dplyr::select(Date, Hour) %>%
    distinct()
  DownloadURLs <- GetDownloadURLs(
    DatesAndHours = UniqueDatesAndHours
  )
  TargetLocationsDatesAndHours <- TargetLocationsDatesAndHours %>%
    merge(
      y = DownloadURLs,
      all.x = T
    )
  if ( any(is.na(TargetLocationsDatesAndHours$DownloadURL))  ) {
    DatesMiss <- unique(TargetLocationsDatesAndHours$Date[is.na(TargetLocationsDatesAndHours$DownloadURL)])
    print(DatesMiss)
    stop("Could not find a download URL for the dates listed above.")
  }

  
  #Loop over download files-----
  RADOLAN_Files_Dir <- file.path(OutDir,"RADOLAN_Files")
  dir.create(RADOLAN_Files_Dir, showWarnings = F)
  UniqueDownloadURLs <- unique(TargetLocationsDatesAndHours$DownloadURL)
  UniqueDownloadURLs <- UniqueDownloadURLs[!is.na(UniqueDownloadURLs)]
  #Loop through files and download file one by one
  for ( i in 1:length(UniqueDownloadURLs) ) {
    #Cleanup-----
    #Clean up temporary folder
    TempFiles <- list.files(path = RADOLAN_Files_Dir, full.names = T)
    if ( length(TempFiles) > 0 ) {
      file.remove(TempFiles)
    }
    #_Download----
    print(paste("Working on download file",i,"of",length(UniqueDownloadURLs)))
    TargetFilePath <- file.path(RADOLAN_Files_Dir,basename(UniqueDownloadURLs[i]))
    if ( file.exists(TargetFilePath) ) {
      print("File already downloaded.")
    } else {
      print("Downloading file...")  
      download.file(
        url = UniqueDownloadURLs[i],
        destfile = TargetFilePath,
        quiet = T
      )
    }
    #_Recursively untar / unzip everything------
    #Daily (recent) files come as archives of 24 hourly files
    #Monthly (historical) files comes as archives of 28-31 daily archives, which themselves
    #contain 24 hourly files each
    #Unzip everything regardless of nesting structure of archives until no tar or gz files
    #are left.
    print("Unzipping...")
    ListArchives <- function(Path) {
      TarFiles <- list.files(
        path = Path,
        full.names = T,
        pattern = "tar$"
      )
      GZFiles <- list.files(
        path = Path,
        full.names = T,
        pattern = "gz$"
      )
      ArchiveFiles <- c(TarFiles, GZFiles)
      return(ArchiveFiles)
    }
    while ( length(ListArchives(RADOLAN_Files_Dir)) > 0 ) {
      CurrentFilePath <- ListArchives(RADOLAN_Files_Dir)[1]
      untar( 
        tarfile = CurrentFilePath,
        exdir = RADOLAN_Files_Dir
      )
      file.remove(CurrentFilePath)
    }
    TempFiles <- list.files(path = RADOLAN_Files_Dir, full.names = T)
    if ( !all(grepl(x = TempFiles, pattern = "asc$")) ) {
      stop("After unzipping everything, only .asc files should remain.")
    }
  
    
    #At this stage, there should only be hourly data files in RADOLAN_Files_Dir
    
    #_Loop over hourly files------
    print("Reading....")
    GetListOfHourlyFiles <- function() {
      HourlyFiles = list.files(
        path = RADOLAN_Files_Dir,
        pattern = "\\d{8}-\\d{4}",
        full.names = T
      )
      return(HourlyFiles)
    }
    #Loop over hourly files
    while( length(GetListOfHourlyFiles() > 0 ) ) {
      CurrentFile <- GetListOfHourlyFiles()[1]
      
      #__Check if current file is required------
      FileName = basename(CurrentFile)
      DigitsOnly = str_extract(
        pattern = "\\d{8}-\\d{4}",
        string = FileName
      )
      CurrentYear = substr(x = DigitsOnly, start = 1, stop = 4)
      CurrentMonth = substr(x = DigitsOnly, start = 5, stop = 6)
      CurrentDay = substr(x = DigitsOnly, start = 7, stop = 8)
      CurrentHour = as.numeric(substr(x = DigitsOnly, start = 10, stop = 11))
      CurrentDate = as.Date(x = paste0(CurrentYear,"-",CurrentMonth,"-",CurrentDay))
      idx_TargetLocations <- which(
        (TargetLocationsDatesAndHours$Date == CurrentDate) &
        (TargetLocationsDatesAndHours$Hour == CurrentHour)
      )
      if ( length(idx_TargetLocations) == 0 ) {
        #If file comes from monthly archive but desired time span starts later
        #or ends earlier during the month
        file.remove(CurrentFile)
        next
      }
      
      #__Read data-----
      #Read current hourly file
      CurrentOutput <- TargetLocationsDatesAndHours[idx_TargetLocations,]
      CurrentOutput$PrecipRADOLAN_mm <- ExtractDataFromFile(
        Coordinates = CurrentOutput[,c("LonEPSG4326","LatEPSG4326")],
        FilePath = CurrentFile
      )
      CurrentOutput <- CurrentOutput %>%
        mutate(
          PrecipRADOLAN_mm = round(PrecipRADOLAN_mm,PrecipPrecision)
        ) %>%
        dplyr::select(LocationLabel, Date, Hour, PrecipRADOLAN_mm)
      if ( any(is.na(CurrentOutput)) ) {
        ErrorFlag <- T
      }
      
      #___Remove current asc file----
      file.remove(CurrentFile)
      # print(CurrentFile)
      

      #__Save output-----
      AppendToFile <- ifelse(
        test = IsFirstSaveOperation,
        yes = F,
        no = T
      )
      WriteColNames <- ifelse(
        test = IsFirstSaveOperation,
        yes = T,
        no = F
      )
      write.table(
        x = CurrentOutput,
        file = OutputFileName,
        sep = ";",
        row.names = F,
        #Write header row only for the first var in first file.
        #Else, just append the data.
        append = AppendToFile,
        col.names = WriteColNames
      )
      IsFirstSaveOperation <- F

    } #End of loop over hourly files
    
  
  } #end of loop over UniqueDownloadURLs
  

} #end of function
