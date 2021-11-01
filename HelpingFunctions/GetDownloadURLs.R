GetDownloadURLs <- function(
  DatesAndHours
) {
  
  #RADOLAN ASCII raster data is provided by DWD in different formats:
  # - folder "recent" contains one archive with 24 hourly files
  # - folder "historical" contains one folder per year with subfolders for each month
  #https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/
  #For each hour in input "DatesAndHours", this script tries to find a matching
  #file in "recent" folder. If it fails, it tries to find a matching file in
  #"historical" folder. If this also fails, DownloadURL is returned as NA for
  #that input row.
  
  BaseURL <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/"
  RecentURL <- paste0(BaseURL,"recent/asc")
  HistoricalURL <- paste0(BaseURL,"historical/asc")
  
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
  
  
  #List all files in "historical" folder-----
  
  #_Get list of years-----
  ContentHistorical <- GetWebsiteContent(
    URL = HistoricalURL
  )
  ContentRowWise <- unlist(strsplit(
    x = ContentHistorical,
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
    stop(paste("Error in function GetDownloadURLs(): No years listed in URL ", HistoricalURL))
  }
  
  #_Get list of monthly files for each year--------
  HistoricalURLs <- data.frame()
  for ( CurrentYear in YearsAvailable) {
    CurrentYearURL <- paste0(HistoricalURL,"/",CurrentYear)
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
        pattern = "\".*.tar\""
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
      HistoricalURLs <- bind_rows(HistoricalURLs,tmp)
    }
  }
  HistoricalURLs <- HistoricalURLs %>%
    drop_na() %>%
    mutate(
      DigitsOnly = str_extract(
        string = FileURL,
        pattern = "[:digit:]{6}"
      ),
      Year = substr(x = DigitsOnly, start = 1, stop = 4),
      Month = substr(x = DigitsOnly, start = 5, stop = 6),
      DownloadURLHistorical = paste0(HistoricalURL,"/",Year,"/",FileURL)
    ) %>%
    dplyr::select(-FileURL, -DigitsOnly)
  
  
  #Match URLs with dates------
  DownloadURLs <- DatesAndHours %>%
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
  
  #_Recent-----
  DownloadURLs <- DownloadURLs %>%
    merge(
      y = RecentURLs,
      all.x = T,
      by = c("Year","Month", "Day")
    )
  
  #_Historical-----
  DownloadURLs <- DownloadURLs %>%
    merge(
      y = HistoricalURLs,
      all.x = T,
      by = c("Year","Month")
    ) %>%
    mutate(
      DownloadURL = case_when(
        !is.na(DownloadURLRecent) ~ DownloadURLRecent,
        !is.na(DownloadURLHistorical) ~ DownloadURLHistorical
      )
    ) %>%
    dplyr::select(Date, Hour, DownloadURL)

  return(DownloadURLs)
}




