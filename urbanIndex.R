cities <- read.csv("citiesGeoCode.csv", encoding = "UTF-8")
frii <- read.csv("frii.csv", encoding ="UTF-8")
library(tidyverse)
library(tidygeocoder)
library(DBI)
library(map)
library(ggplot2)

db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
urbInd <- dbGetQuery(db,"SELECT * from urbanindex")
dbDisconnect(db)
length(unique(getCities$city))
a <- (distinct(urbInd[,-1]))
a$city[duplicated(a$city)]
urbInd$population <-  as.numeric(gsub(",", ".", urbInd$population))

unique(urbInd$citytype)
urbInd <- urbInd %>% 
    mutate(urbInd, population = ifelse(
                      citytype == "Крупнейший город", 
                      population * 1000000, population * 1000))
dist()
countyMean <- urbInd %>%
    group_by(county) %>%
    summarise(meanInd = mean(as.numeric(combindex))) %>%
    arrange(desc(meanInd))

a <- geocode(urbInd, address = county, method = "osm")
a <- left_join(a, countyMean)

rus <- map_data("world", region = "Russia")
ggplot(rus, aes(x = long, y = lat)) +
    geom_polygon(aes( group = group, fill = region))+
    geom_point(data = a, aes(x = long, y = lat, size = meanInd, color = meanInd)) +
    theme_void()


boxplot(as.numeric(combindex) ~ county, a)
abline(h = mean(as.numeric(a$combindex)), col = "red")
summary(as.numeric(a$combindex))
