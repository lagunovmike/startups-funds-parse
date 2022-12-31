suppressMessages(library(rvest))
suppressMessages(library(tidyverse))
suppressMessages(library("RSelenium"))
suppressMessages(library(DBI))



rD <- rsDriver(port = netstat::free_port(), browser = "firefox", verbose = FALSE)
remDr <- rD$client

parseSK <- function(from, to){
    link <- "https://navigator.sk.ru/navigator/#/orn/"
    comp <- list()
    cnt <- 0
    cnt_success <- 0
    listElem <- 1
    for(i in from:to){
        pageLink <- paste0(link, i)
        remDr$navigate(pageLink)
        remDr$refresh()
        Sys.sleep(1)
        getUrl <- read_html(remDr$getPageSource()[[1]])
        
        skID <- i
        
        skName <- getUrl %>%
            html_nodes(xpath = '//*[@id="root"]/div[2]/div/div/div/div[1]/div[1]/div[1]/h1') %>%
            html_text()
        
        if(nchar(skName) <= 1){
            cat(i, "\r")
            next()
        }
        
        skAddress <- getUrl %>%
            html_nodes(".contact-item.icon-row.icon-row_33.icon-row_address") %>%
            html_text()
        skCluster <- getUrl %>%
            html_nodes(".page-header__cluster-title") %>%
            html_text
        
        skEst <- getUrl %>%
            html_nodes(xpath = '//*[@id="root"]/div[2]/div/div/div/div[1]/div[1]/div[2]/div[2]/div[1]/div[2]') %>%
            html_text
        
        
        comp[[listElem]] <- c(skID, skName, skAddress, skCluster, skEst)
        listElem <- listElem + 1
        cnt <- cnt + 1
        cnt_success <- cnt_success +1
        if(cnt == 4){
            companiesDF <- as.data.frame(do.call(rbind, Filter(length, comp)))
            names(companiesDF) <- c("ID", "name", "address", "cluster", "est")
            if(dbCanConnect(RSQLite::SQLite(), dbname = "Startups.db") == TRUE){
                db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
            } else{
                Sys.sleep(3)
                db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
            }
            dbAppendTable(db, name = "skolkovo", value = companiesDF)
            dbDisconnect(db)
            comp <- list()
            cnt <- 0
            cat("sent to DB", "| Passed:",i-from, "\n")
        }
        cat(i, "|", cnt_success, "\r")
    }
    return(comp)
}
#debug(parseSK)

db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
from <- as.vector(dbGetQuery(db, "SELECT MAX(ID) as max FROM skolkovo")$max) + 1
dbDisconnect(db)
to <- 1125000

res <- parseSK(from, to)

remDr$close()
rD$server$stop()
rm(rD)