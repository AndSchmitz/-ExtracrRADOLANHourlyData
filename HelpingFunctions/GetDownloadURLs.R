GetDownloadURLs <- function(
  DatesToDownload
) {
  
  library(httr) #for extracting download URLs from DWD CDC websites
  
  #RADOLAN ASCII raster data is provided by DWD in three different formats:
  # - recent (newest data)
  # - reproc (improved/valided(?) data) with DOI for methods
  # - historical (unhanged historical data?)
  #
  #This script uses "reproc" data when available or else "recent". "historical"
  #is not used. "reproc" comes as one folder per year with subfolders for each
  #month folder. "recent" contains daily archives with 24 hourly files.
  
  BaseURL <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/"
  RecentURL <- paste0(BaseURL,"recent/asc")
  ReprocURL <- paste0(BaseURL,"reproc/2017_002/asc")
  
  #Helping function to get website content------
  GetWebsiteContent <- function(
    URL
  ) {
    
    #Get http response
    httrResponse <- httr::GET(url  = URL)
    
    #Extract website content from http response
    WebSiteContent <- content(
      x = httrResponse,
      as = "text",
      encoding = "UTF8"
    )
    
    return(WebSiteContent)
  } #end of function GetWebsiteContent()
  

  #List all files in "recent" folder-----
  ContentRecent <- GetWebsiteContent(
    URL = RecentURL
  )

  #Pattern matching tweaked to match recent folder
  #https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/recent/asc/
  GZFileNames <- str_extract_all(
    string = ContentRecent,
    pattern = "href.+?tar\\.gz"
  )
  GZFileNames <- sapply(
    FUN = gsub,
    pattern = "href=\"",
    replacement = "",
    X = GZFileNames,
    fixed = FALSE
  )
  RecentURLs <- data.frame(
    BaseFileName = GZFileNames
  ) %>%
    mutate(
      DownloadURLRecent = paste0(RecentURL,"/",BaseFileName),
      DateString = str_extract(
        string = BaseFileName,
        pattern = "-[:digit:]+"
      ),
      DateString = gsub(
        x = DateString,
        pattern = "-",
        replacement = ""
      ),
      Year = as.numeric(substr(
        x = DateString,
        start = 1,
        stop = 4
      )),
      Month = as.numeric(substr(
        x = DateString,
        start = 5,
        stop = 6
      )),
      Day = as.numeric(substr(
        x = DateString,
        start = 7,
        stop = 8
      ))
    ) %>%
    dplyr::select(-BaseFileName, -DateString)
  #FIXME? 2022-02-24
  #Folder "recent" also contains some single files from previous years (2007
  #2016,..). Why? These files are also available in the "reproc" folder, i.e.
  #these dates are covered twice.
  #Workaround: Keep only current year and past year in "RecentURLs"
  ValidRecentYears <- as.numeric(format(Sys.Date(),"%Y"))
  ValidRecentYears <- (ValidRecentYears-1):ValidRecentYears
  RecentURLs <- RecentURLs %>%
    filter(
      Year %in% ValidRecentYears
    )
  
  #List all files in "reproc" folder-----
  
  #_Get list of years-----
  ContentReproc <- GetWebsiteContent(
    URL = ReprocURL
  )
  ContentRowWise <- unlist(strsplit(
    x = ContentReproc,
    split = "\n"
  ))
  YearsAvailable <- vector()
  for ( i in 1:length(ContentRowWise) ) {
    CurrentRowContent <- ContentRowWise[i]
    CurrentYear = str_extract(
      string = CurrentRowContent,
      pattern = "[:digit:][:digit:][:digit:][:digit:]"
    )
    YearsAvailable <- c(YearsAvailable, CurrentYear)
    YearsAvailable <- YearsAvailable[!is.na(YearsAvailable)]
  }
  if ( length(YearsAvailable) == 0 ) {
    stop(paste("Error in function GetDownloadURLs(): No years listed in URL ", ReprocURL))
  }
  
  #_Get list of monthly files for each year--------
  ReprocURLs <- data.frame()
  for ( CurrentYear in YearsAvailable) {
    CurrentYearURL <- paste0(ReprocURL,"/",CurrentYear)
    ContentCurrentYear <- GetWebsiteContent(
      URL = CurrentYearURL
    )
    ContentRowWise <- unlist(strsplit(
      x = ContentCurrentYear,
      split = "\n"
    ))
    for ( CurrentRow in ContentRowWise ) {
      FileURL = str_extract(
        string = CurrentRow,
        pattern = "\".*.tar.gz\""
      )
      if ( is.na(FileURL) ) {
        next
      }
      FileURL <- gsub(
        x = FileURL,
        pattern = "\"",
        replacement = ""
      )
      tmp <- data.frame(
        FileURL = FileURL
      )
      ReprocURLs <- bind_rows(ReprocURLs,tmp)
    }
  }
  ReprocURLs <- ReprocURLs %>%
    drop_na() %>%
    mutate(
      DigitsOnly = str_extract(
        string = FileURL,
        pattern = "[:digit:]{6}"
      ),
      Year = as.numeric(substr(x = DigitsOnly, start = 1, stop = 4)),
      Month = as.numeric(substr(x = DigitsOnly, start = 5, stop = 6)),
      DownloadURLReproc = paste0(ReprocURL,"/",Year,"/",FileURL)
    ) %>%
    dplyr::select(-FileURL, -DigitsOnly)
  
  
  #Match URLs with dates------
  DownloadURLs <- DatesToDownload %>%
    mutate(
      Year = as.numeric(format(
        x = Date,
        format = "%Y"
      )),
      Month = as.numeric(format(
        x = Date,
        format = "%m"
      )),
      Day = as.numeric(format(
        x = Date,
        format = "%d"
      ))
    )
  
  #_Merge recent and reproc----
  DownloadURLs <- DownloadURLs %>%
    merge(
      y = RecentURLs,
      all.x = T,
      by = c("Year","Month", "Day")
    ) %>%
    merge(
      y = ReprocURLs,
      all.x = T,
      by = c("Year","Month")
    ) %>%
    mutate(
      DownloadURL = case_when(
        !is.na(DownloadURLRecent) & is.na(DownloadURLReproc) ~ DownloadURLRecent,
        is.na(DownloadURLRecent) & !is.na(DownloadURLReproc) ~ DownloadURLReproc,
        #Default to recent: 2022-02-24: file "2021-01-01" if with only 5 hours
        #in "reproc" but with 24 hours (complete) in recent.
        !is.na(DownloadURLRecent) & !is.na(DownloadURLReproc) ~ DownloadURLRecent,
        T ~ NA_character_
      )
    ) %>%
    dplyr::select(Date, DownloadURL)

  return(DownloadURLs)
}




