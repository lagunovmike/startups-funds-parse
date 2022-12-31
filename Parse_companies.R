library(rvest)
library(tidyverse)
library(DBI)
link <- "https://innmind.com/startups/"



x <- function(from){
  cnt <- 0
  comp <- list()
  for (i in (from+2):12000){
    page <- paste0(link, i)
    getPage <- tryCatch(read_html(page), error = function(e){404})
    if(is.numeric(getPage)){
      cat(i, "\r")
      next
    }
    
    IsStartPage <- getPage %>%
      html_nodes(xpath = "/html/body/div[1]/div/div[1]/div[1]/div/div/div[4]/div[1]/p") %>%
      html_text()
    
    if(length(IsStartPage != 0)){
      cat(i, "\r")
      next
    }
    
    compID <- i
    
    compName <- getPage %>%
      html_nodes(xpath = "/html/body/div[1]/div/div[1]/div/div/div[3]/div/div[2]/h4") %>%
      html_text()
    
    compAddress <- getPage %>%
      html_nodes(xpath = "/html/body/div[1]/div/div[2]/div/div/div/div/div[2]/div[1]/div/div") %>%
      html_text()
    
    comp[[i]] <- c(compID, compName, compAddress)
    
    cnt <- cnt + 1
    if(cnt == 10){
      companiesDF <- as.data.frame(do.call(rbind, Filter(length, comp)))
      names(companiesDF) <- c("ID", "name", "address")
      if(dbCanConnect(RSQLite::SQLite(), dbname = "Startups.db") == TRUE){
          db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
      } else{
          Sys.sleep(3)
          db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
      }
      dbAppendTable(db, name = "companies", value = companiesDF)
      dbDisconnect(db)
      comp <- list()
      cnt <- 0
      cat("sent to DB", "\n")
      
    }
    cat(cnt, "\r")
  }
  return(i)
}

db <- dbConnect(RSQLite::SQLite(), dbname = "Startups.db")
max_id_db <- as.vector(dbGetQuery(db, "SELECT MAX(ID) as max FROM companies")$max)
dbDisconnect(db)

funStart <- Sys.time()
companies <- x(max_id_db)
vfunFinish <- Sys.time()
funFinish - funStart



#companiesDF <- as.data.frame(do.call(rbind, Filter(length, companies)))
#companiesDF <- separate(companiesDF, "V2", c("Country", ))

#strsplit(companiesDF$V2, ",")[[11]]