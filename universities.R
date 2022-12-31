library(tidyverse)
library(rvest)
library(RSelenium)
library(leaflet)
link <- read_html("https://raex-rr.com/education/universities/rating_of_universities_of_russia#table")

title <- link %>%
    html_nodes(".member_name_row") %>%
    html_text()

title <- gsub("\n", "", title)
title <- str_trim(title)

tail(rank)

rank <- link %>%
    html_nodes(".member_indicator_number.r_cell") %>%
    html_text()

infrastructure <- character()
for(i in 2:101){
    xpathLink <- paste0('//*[@id="carouselExampleIndicators"]/div/div[', i, ']/div[7]')
    infrastructure[[i]] <- link %>%
        html_nodes(xpath = xpathLink) %>%
        html_text()
    
}


alumniDemand <- character()
for(i in 2:101){
    xpathLink <- paste0('//*[@id="carouselExampleIndicators"]/div/div[', i, ']/div[8]')
    alumniDemand[[i]] <- link %>%
        html_nodes(xpath = xpathLink) %>%
        html_text()
}

scienceLevel <- character()
for(i in 2:101){
    xpathLink <- paste0('//*[@id="carouselExampleIndicators"]/div/div[', i, ']/div[9]')
    scienceLevel[[i]] <- link %>%
        html_nodes(xpath = xpathLink) %>%
        html_text()
}

head(scienceLevel)

infrastructure <- infrastructure[-1]
alumniDemand <- alumniDemand[-1]
scienceLevel <- scienceLevel[-1]

infrastructure <- gsub("Условия для получения качественного образования, ранг", "", infrastructure)
alumniDemand <- gsub("Уровень востребованности выпускников работодателями, ранг", "", alumniDemand)
scienceLevel <- gsub("Уровень научно-исследовательской деятельности, ранг", "", scienceLevel)


universities <- data.frame(rank = seq(1:100), title = title, 
                           infrastructure = infrastructure,
                           alumnidemand = alumniDemand,
                           sciencelevel = scienceLevel)



#readr::write_excel(universities, "universities.csv")
#uni2 <- readr::read_csv("universities.csv")


rD <- rsDriver(port = netstat::free_port(), browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$navigate("https://developers-dot-devsite-v2-prod.appspot.com/maps/documentation/utils/geocoder")

geo <- list()
for(i in 1:nrow(universities)){
    webElem <- remDr$findElement("class", "pac-target-input")
    webElem$clickElement()
    webElem$clearElement()
    webElem$sendKeysToElement(list(universities$title[i]))
    sendTitle <- remDr$findElement("id", "geocode-button")
    sendTitle$clickElement()
    Sys.sleep(2)
    uniGeo <- read_html(remDr$getPageSource()[[1]])
    
    getGeo <- uniGeo %>%
        html_nodes(xpath = '//*[@id="result-0"]/table/tbody/tr/td[2]/p[3]') %>%
        html_text()
    if(length(getGeo) == 0) {
        geo[[i]] <- c(university = universities$title[i], lat = NA, lon = NA)
        next
    }
    geoTemp <- regmatches(getGeo, gregexpr("[[:digit:]]+.[[:digit:]]+", getGeo))[[1]]
    geo[[i]] <- c(university = universities$title[i], lat = geoTemp[1], lon = geoTemp[2])
    Sys.sleep(2)
    cat("Done:", i, "\r")
}
unGeo <- map_df(geo, bind_rows)
unGeo[34,2] <- "55.857328"
unGeo[34,3] <- "37.691449"
unGeo[,2] <- as.numeric(unGeo$lat)
unGeo[,3] <- as.numeric(unGeo$lon)
unGeo %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(popup = paste("Name:",unGeo$university)) %>%
    addMarkers(sk$lat, sk$lon)
