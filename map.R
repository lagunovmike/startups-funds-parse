library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
library(ggmap)
library(ggalt)
Sys.setlocale("LC_ALL", "russian")
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill="lightgray", colour = "white") +
    theme_void()
ru <- c("Russia")
ru.maps <- map_data("world", region = ru)
region.lab.data <- ru.maps %>%
    group_by(region) %>%
    summarise(long = mean(long), lat = mean(lat))

ggplot(ru.maps, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), fill = "#d7f0ad") +
    geom_point(data = frii, aes(lon, lat, size = multiply), color = "orange", alpha = 0.6) +
    scale_fill_viridis_d() +
    theme_void() +
    theme(legend.position = "none")


frii <- read.csv("frii.csv", encoding = "UTF-8")
frii$multiply <- frii$lat * frii$lon
frii$multiply <- factor(frii$multiply)
msk <- filter(frii, city == "Москва")


freq <- frii %>%
    group_by(multiply) %>%
    summarise(freq = n(), .groups = "drop") %>%
    arrange(desc(freq)) %>%
    unique()
frii$multiply <- relevel(frii$multiply, )
length(freq$multiply)
freq$multiply
