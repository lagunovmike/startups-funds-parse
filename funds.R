library(readr)
library(leaflet)
library(jsonlite)
library(XML)
funds <- readxl::read_excel("vc funds.xlsx", range = "A1:C37")
Sys.setlocale("LC_CTYPE", "english")
cGeoCode <- read.csv("citiesGeoCode.csv", fileEncoding = "UTF-8")

funds$lat <- NA
funds$lon <- NA

for(i in 1:nrow(funds)){
    if(is.na(funds$city[i])){
        funds$lat[i] <- NA
        funds$lon[i] <- NA
    } else{
        funds$lat[i] <- cGeoCode[which(funds$city[i] == cGeoCode$city)[1], 3]
        funds$lon[i] <- cGeoCode[which(funds$city[i] == cGeoCode$city)[1], 4]    
    }
    
}
cGeoCode[which(funds$city[2] == cGeoCode$city), 3]

# Fix Нижний Новгород
funds$lat[29] <- cGeoCode$lat[3]
funds$lon[29] <- cGeoCode$lon[3]

funds %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(popup = paste("Name:",funds$fund,"| City:", funds$city),
               clusterOptions = markerClusterOptions()
               )
