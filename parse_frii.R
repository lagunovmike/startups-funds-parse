library(rvest)
suppressMessages(library(tidyverse))
suppressMessages(library("RSelenium"))
Sys.setlocale("LC_CTYPE", "russian") #solve encoding issues
library(leaflet)


acceleratorLink <- "https://www.iidf.ru/fond/projects/"
seedLink <- "https://www.iidf.ru/fond/projects/?ROUND=seed"
roundALink <- "https://www.iidf.ru/fond/projects/?ROUND=rounda"



## Accelerator
# load accelerator page
rD <- rsDriver(port = netstat::free_port(), browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$navigate(acceleratorLink)

replicate(50, {
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    morereviews <- remDr$findElement(using = "xpath", '//*[@id="load-more"]')
    morereviews$clickElement()
    Sys.sleep(3)
})

# read page's source code
getFull <- read_html(remDr$getPageSource()[[1]])

# Parse accelerator
accelerator <- getFull %>%
    html_nodes(".list__item") %>%
    map_df(~list(title = html_nodes(.x, ".project-case-item__title > a") %>%
                     html_text() %>%
                     {if (length(.) == 0) NA else .},
                 city = html_nodes(.x, ".project-case-item__address") %>%
                     html_text() %>%
                     {if (length(.) == 0) NA else .},
                 webside = html_nodes(.x, ".project-case-item__title > a") %>%
                     html_attr("href") %>%
                     {if (length(.) == 0) NA else .}
                 ))

accelerator <- accelerator[rowSums(is.na(accelerator)) != ncol(accelerator),]
accelerator <- accelerator[-which(is.na(accelerator$title)),]
accelerator <- mutate(accelerator, round = "accelerator")


accelerator %>%
    group_by(city) %>%
    summarize(freq = n(), .groups = "drop") %>%
    arrange(desc(freq)) %>%
    mutate(prop = freq/sum(freq), cumsum = cumsum(prop))


#######
# SEED
#######
remDr$navigate(seedLink)

replicate(10, {
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    morereviews <- remDr$findElement(using = "xpath", '//*[@id="load-more"]')
    morereviews$clickElement()
    Sys.sleep(3)
})

# read page's source code
getFull <- read_html(remDr$getPageSource()[[1]])


seed <- getFull %>%
    html_nodes(".list__item") %>%
    map_df(~list(title = html_nodes(.x, ".project-case-item__title > a") %>%
                     html_text() %>%
                     {if (length(.) == 0) NA else .},
                 city = html_nodes(.x, ".project-case-item__address") %>%
                     html_text() %>%
                     {if (length(.) == 0) NA else .},
                 webside = html_nodes(.x, ".project-case-item__title > a") %>%
                     html_attr("href") %>%
                     {if (length(.) == 0) NA else .}
    ))

seed <- seed[rowSums(is.na(seed)) != ncol(seed),]
seed <- seed[which(!is.na(seed$title)),]
seed$city <- gsub("\n", "", seed$city)
seed$city <- str_trim(seed$city)
seed <- mutate(seed, round = "seed")


#####
# Round A
#####

remDr$navigate(roundALink)

# Parse
replicate(10, {
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    morereviews <- remDr$findElement(using = "xpath", '//*[@id="load-more"]')
    morereviews$clickElement()
    Sys.sleep(3)
})

getFull <- read_html(remDr$getPageSource()[[1]])


roundA <- getFull %>%
    html_nodes(".list__item") %>%
    map_df(~list(title = html_nodes(.x, ".project-case-item__title > a") %>%
                     html_text() %>%
                     {if (length(.) == 0) NA else .},
                 city = html_nodes(.x, ".project-case-item__address") %>%
                     html_text() %>%
                     {if (length(.) == 0) NA else .},
                 webside = html_nodes(.x, ".project-case-item__title > a") %>%
                     html_attr("href") %>%
                     {if (length(.) == 0) NA else .}
    ))

roundA <- roundA[rowSums(is.na(roundA)) != ncol(roundA),]
roundA <- roundA[which(!is.na(roundA$title)),]
roundA$city <- gsub("\n", "", roundA$city)
roundA$city <- str_trim(roundA$city)
roundA <- mutate(roundA, round = "seed")


frii <- rbind(accelerator, seed, roundA)
frii$city <- gsub("\n", "", frii$city)
frii$city <- str_trim(frii$city)


byCity <- frii %>%
    filter(city != is.na(city)) %>%
    group_by(city) %>%
    summarise(freq = n(), .groups = "drop") %>%
    arrange(desc(freq)) %>%
    mutate(prop = round(freq/sum(freq),3), cumsum = cumsum(prop))
byCity

geoCodeParse <- list()
for (i in 1:nrow(frii)){
    city <- frii$city[i] 
    link <- paste0("https://nominatim.openstreetmap.org/search.php?q=", city, "&format=jsonv2")
    getGeo <- jsonlite::fromJSON(link)
    geoCodeParse[[i]] <- c(lat = getGeo$lat[1], lon = getGeo$lon[1])
    cat("Done:", i, "\r")
    Sys.sleep(1)
}

l <- lapply(geoCodeParse, function(x){if(is.null(x)) data.frame(lat = NA, lon = NA) 
    else x})
geoCode <- map_df(l, bind_rows)
geoCode <- dplyr::bind_rows(geoCodeParse)
geoCode[which(geoCode$lon == "11.52803643954819"),] <- NA
geoCode <- sapply(geoCode, as.numeric)

frii <- cbind(frii,geoCode)

# Delete Алматы
frii <- frii[-which(frii$city == "Алматы"),]


write.csv(frii, "frii.csv", fileEncoding = "UTF-8")


frii %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(popup = paste(frii$title, frii$city), 
               clusterOptions = markerClusterOptions())
