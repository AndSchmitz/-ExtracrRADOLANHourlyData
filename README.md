# ExtractRADOLANHourlyData
This script extracts [DWD RADOLAN hourly precipitation amounts](https://www.dwd.de/DE/leistungen/radolan/radolan_info/home_freie_radolan_kartendaten.html) at point locations. 

## How to use
 - Download all files from this repository (e.g. via Code -> Download ZIP above).
 - Extact the ZIP file. The directory where "ExtractRADOLANHourlyData.R" is stored is called "working directory" in the following.
 - Make sure the file "TargetLocationsAndTimeSpans.csv" is stored in a subfolder "Input" of the working directory.
 - Make sure all other .R files are stored in a subfolder "HelpingFunctions" of the working directory.
 - Install all libraries listed in the beginning of "ExtractRADOLANHourlyData.R".
 - Adjust the variable "WorkDir" in the beginning of "ExtractRADOLANHourlyData.R" to match the working directory.
 - Run the script.
 - Adjust the "TargetLocationsAndTimeSpans.csv" to your needs.

## Validation
The values extracted with the R script have been compared to values extracted with the [radolan2map QGIS plugin](https://gitlab.com/Weatherman_/radolan2map/-/wikis/home). Precipitation amounts at four locations for 2021-01-01 00:50 (ASCII raster file RW-20210101.tar.gz from [here](https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/)) have been compared. The results are shown below. The difference observed for location C is likely related to interpolation during extraction from raster data. The spatial projection for the raster data has been taken from page 17 in [this pdf](https://opendata.dwd.de/climate_environment/CDC/help/RADOLAN/Unterstuetzungsdokumente/Unterstuetzungsdokument_Verwendung_von_RADOLAN_RADKLIM_Produkten_in_GIS_Software.pdf).

| LocationLabel | LonEPSG4326 | LatEPSG4326 | Date     | Hour  | Value from QGIS | Value from R |
| ------------- | ----------- | ----------- | -------- | ----- | --------------- | ------------ |
| A             | 7.9008635   | 47.35069782 | 1.1.2021 | 00:50 | 1.29999         | 1.30104      |
| B             | 8.92487     | 50.99709    | 1.1.2021 | 00:50 | 1.29999         | 1.298944     |
| C             | 11.66849    | 54.72045    | 1.1.2021 | 00:50 | 3.59999         | 3.473923     |
| D             | 14.5334     | 53.609      | 1.1.2021 | 00:50 | 0.3             | 0.3          |
