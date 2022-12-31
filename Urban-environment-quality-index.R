# Urban environment quality index

library(RSelenium)
library(rvest)
library(dplyr)
library(DBI)
Sys.setlocale("LC_CTYPE", "russian")
rD <- rsDriver(port = netstat::free_port(), browser = "firefox", verbose = FALSE)
remDr <- rD$client
urbanQltInd <- list()
startTime <- Sys.time()
cnt <- 0
todo <- setdiff(seq(1:3000), haveId$id)
todo <- todo[todo >= 2250]
for(i in todo){
    link <- paste0("https://xn----dtbcccdtsypabxk.xn--p1ai/#/cities/", i)
    remDr$navigate(link)
    remDr$refresh()
    Sys.sleep(1)
    parsedPage <- read_html(remDr$getPageSource()[[1]])
    
    city <- parsedPage %>%
        html_nodes(".sc-eNQAEJ.sc-bsbRJL.ejmNcS") %>%
        html_text()
    if(nchar(city) == 0 | length(city) == 0){
        cat("Done S:", i, "\r")
        next
    }
    
    county <- parsedPage %>%
        html_nodes(".sc-jTzLTM.sc-hZSUBg.isNPgl") %>%
        html_text()
    cityType <- parsedPage %>%
        html_nodes(xpath = '//*[@id="root"]/div/section/div/div[2]/div[1]') %>%
        html_text()
    population <- parsedPage %>%
        html_nodes(xpath = '//*[@id="root"]/div/section/div/div[2]/div[3]') %>%
        html_attr("data-icon")
    combIndex <- parsedPage %>%
        html_nodes(".sc-eNQAEJ.sc-cLQEGU.hzYnoD") %>%
        html_text()
    
    indexComponents <- parsedPage %>%
        html_nodes(".sc-fOKMvo.cUcjfr") %>%
        html_text()

    
    
    urbanQltInd[[i]] <- data.frame(id = i,
                                   city = city,
                                   county = county,
                                   citytype = cityType,
                                   population = population,
                                   combindex = combIndex,
                                   livingQuality = indexComponents[1],
                                   streetNetwork = indexComponents[2],
                                   greenSpaces = indexComponents[3],
                                   businessInfrastructure = indexComponents[4],
                                   socialInfrastructure = indexComponents[5],
                                   citywideSpace = indexComponents[6])
    getTime <- difftime(Sys.time(), startTime, units = "mins")
    
    if(cnt == 5){
        urbanIndexDF <- purrr::map_df(urbanQltInd, bind_rows)
        db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
        dbAppendTable(db, "urbanindex", urbanIndexDF)
        dbDisconnect(db)
        cnt <- 0
        urbanQltInd <- list()
    }
    cnt <- cnt + 1
    cat("Done:", i, "| Minutes:", floor(getTime), "\r")
}
urbanIndexDF <- purrr::map_df(urbanQltInd, bind_rows)


haveId <- dbGetQuery(db,"SELECT id from urbanindex")
