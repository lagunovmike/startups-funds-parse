library(DBI)
library(tidyverse)
library(plotly)
Sys.setlocale("LC_CTYPE", "russian")

db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
companies <- dbGetQuery(db, "SELECT *
                        FROM companies")
dbDisconnect(db)

addr <- strsplit(companies$address, ",")
compCountry <- map_chr(addr,1)
compCity <- map_chr(addr,2)

companies <- companies %>%
  mutate(country = str_trim(compCountry), city = str_trim(compCity))

compRus <- companies %>%
  filter(country == "Russia")

tbl <- compRus %>%
  group_by(city) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq)) %>%
  mutate(prop = round(freq/sum(freq),2))
barplot(freq ~ city, tbl)

print(tbl)

plot_ly(data = tbl, x = ~city, y = ~freq, type = "bar")

companies %>% select(name, city) %>%
  filter(city == "Krasnodar")

write.csv(companies, "companies.csv", fileEncoding = "utf-8")
