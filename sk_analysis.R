library(DBI)
library(tidyverse)
library(gdata)
library(leaflet)
Sys.setlocale("LC_CTYPE", "russian")
setwd("C:/Projects/DataScience/Sandbox/Startups-info")
db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
sk <- dbGetQuery(db, "SELECT * FROM skolkovo")
dbDisconnect(db)


city <- regmatches(sk$address, gregexpr("ГОРОД (\\S+)", sk$address))
city <- gsub(",", "", city)
city <- gsub("ГОРОД ", "", city)
city[which(city == "character(0)")] <- NA

byFreq <- arrange(as.data.frame(table(city)), desc(Freq)) %>%
    filter(city != is.na(city)) %>%
    mutate(prop = round(Freq/sum(Freq), 3), cumsum = cumsum(prop)) %>%
    select(city)


sk <- cbind(sk, city)
#sk <- na.omit(sk)
sk$city <- reorder.factor(sk$city, new.order = byFreq$city)
sk <- sk %>% arrange(city)

sk$name <- str_to_title(sk$name)
sk$address <- str_to_title(sk$address)
sk$city <- str_to_title(sk$city)

#write.csv(sk, "sk.csv", fileEncoding = "utf-8")



link <- paste0("https://nominatim.openstreetmap.org/search.php?q=", sk$city[1], "&format=jsonv2")
getGeo <- jsonlite::fromJSON(link)
c(getGeo$lat[1], getGeo$lon[1])
sk1 <- sk
sk1$lat <- NA
geoCodeParse <- list()

for (i in 1:length(unique(sk$city))){
    tempCity <- unique(sk$city)[i] 
    link <- paste0("https://nominatim.openstreetmap.org/search.php?q=", tempCity, "&format=jsonv2")
    getGeo <- jsonlite::fromJSON(link)
    geoCodeParse[[i]] <- c(city = tempCity, lat = getGeo$lat[1], lon = getGeo$lon[1])
    cat("Done:", i, "\r")
    Sys.sleep(.8)
}

l <- lapply(geoCodeParse, function(x){if(is.null(x)) data.frame(lat = NA, lon = NA) 
    else x})
geoCode <- map_df(l, bind_rows)
geoCode <- dplyr::bind_rows(geoCodeParse)

for(i in 1:nrow(sk)){
    if(is.na(sk$city[i])){
        sk$lat[i] <- NA
        sk$lon[i] <- NA
    } else{
        sk$lat[i] <- geoCode[which(sk$city[i] == geoCode$city), 2]
        sk$lon[i] <- geoCode[which(sk$city[i] == geoCode$city), 3]
    }
}

## Get precise location coordinates
library(RSelenium)
rD <- rsDriver(port = netstat::free_port(), browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$navigate("https://developers-dot-devsite-v2-prod.appspot.com/maps/documentation/utils/geocoder")

presCoord <- list()
for(i in 1:nrow(sk)){
    webElem <- remDr$findElement("class", "pac-target-input")
    webElem$clickElement()
    webElem$clearElement()
    webElem$sendKeysToElement(list(sk$address[i]))
    sendTitle <- remDr$findElement("id", "geocode-button")
    sendTitle$clickElement()
    Sys.sleep(2)
    uniGeo <- read_html(remDr$getPageSource()[[1]])
    
    getGeo <- uniGeo %>%
        html_nodes(xpath = '//*[@id="result-0"]/table/tbody/tr/td[2]/p[3]') %>%
        html_text()
    if(length(getGeo) == 0) {
        preS[[i]] <- c(startup = sk$name[i], lat = NA, lon = NA)
        next
    }
    geoTemp <- regmatches(getGeo, gregexpr("[[:digit:]]+.[[:digit:]]+", getGeo))[[1]]
    geo[[i]] <- c(startup = sk$name[i], lat = geoTemp[1], lon = geoTemp[2])
    Sys.sleep(2)
    cat("Done:", i, "\r")
}
GeoDF <- map_df(geo, bind_rows)
sk$cityLat <- sk$lat
sk$cityLon <- sk$lon

sk$lat <- NA
sk$lon <- NA
for(i in 1:nrow(sk)){
    sk$lat[i] <- as.data.frame(GeoDF)[which(sk$name[i] == GeoDF$startup)[1], 2]
    sk$lon[i] <- as.data.frame(GeoDF)[which(sk$name[i] == GeoDF$startup)[1], 3]
}
which(is.na(sk$lat))
## Finish getting precise location coordinates

## Update DB
db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
dbID <- dbGetQuery(db, "SELECT id from skolkovo")
for(i in 1:nrow(sk)){
    dbExecute(db, "UPDATE skolkovo SET lat = ?, lon = ?,
              cityLat = ?, cityLon = ? WHERE id == ?;", 
              params = c(sk$lat[i], sk$lon[i], sk$cityLat[i], 
                         sk$cityLon[i], dbID$ID[i]))
    cat("done:", i, "\r")
    
}
dbDisconnect(db)

sk$lat <- as.numeric(sk$lat)
sk$lon <- as.numeric(sk$lon)



sk %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(popup = paste("Name:",sk$name,"| City:", sk$city),
               clusterOptions = markerClusterOptions())



# Make a file with geo code for cities

citiesGeoCode <- data.frame(city = unique(sk$city))

for(i in 1:nrow(citiesGeoCode)){
    citiesGeoCode$lat[i] <- sk$lat[which(citiesGeoCode$city[i] == sk$city)[1]]
    citiesGeoCode$lon[i] <- sk$lon[which(citiesGeoCode$city[i] == sk$city)[1]]
}
write.csv(citiesGeoCode, "citiesGeoCode.csv", fileEncoding = "UTF-8")
