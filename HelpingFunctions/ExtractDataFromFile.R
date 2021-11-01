ExtractDataFromFile <- function(Coordinates, FilePath){

  #Coordinates: Dataframe with coordinates where to extract data.
  #Columns LonEPSG4326, LatEPSG4326
  #
  #FilePath: Full path to the file.
  if ( !("data.frame" %in% class(Coordinates)) ) {
    stop("Coordinates must be of class data.frame")
  }
  if ( !("LonEPSG4326" %in% colnames(Coordinates)) | !("LatEPSG4326" %in% colnames(Coordinates)) ) {
    stop("Columns LonEPSG4326 and LatEPSG4326 must be in input data frame.")
  }
  Coordinates <- Coordinates %>%
    dplyr::select(LonEPSG4326, LatEPSG4326)
  
  
  #Read raster
  Raster <- raster(
    x = FilePath
  )
  
  
  #Set projection
  #taken from page 17 in
  #https://opendata.dwd.de/climate_environment/CDC/help/RADOLAN/Unterstuetzungsdokumente/Unterstuetzungsdokument_Verwendung_von_RADOLAN_RADKLIM_Produkten_in_GIS_Software.pdf
  suppressWarnings(
    raster::crs(Raster) <- "+proj=stere +lat_0=90.0 +lon_0=10.0 +lat_ts=60.0 +a=6370040 +b=6370040 +units=m"
  )


  #Reproject raster
  RasterEPSG4326 <- raster::projectRaster(
    from = Raster,
    crs = CRS("+init=epsg:4326")
  )
  
  
  #Convert points to spatial points object
  CoordsSP <- SpatialPoints(
    #First column is treated as lon
    #Second column is treated as lat
    coords = Coordinates,
    proj4string = CRS("+init=epsg:4326")
  )
  
  
  #Extract values at points
  #without interpolation
  ValuesExtracted <- raster::extract(
    x = RasterEPSG4326,
    y = CoordsSP,
    method = "simple"
  ) / 10
  
  
  return(ValuesExtracted)
  
}




