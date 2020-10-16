library(DBI)
library(RPostgres)
library(dplyr)
library(magrittr)



# Preliminaries
setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()



# Connect to database
db <- "poshmark" # provide the name of your db
host_db <- "localhost" # i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
db_port <- "5432" # or any other port specified by the DBA
db_user <- "postgres"
db_password <- "Poshmark"

con <- dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)
dbListTables(con)

# Load data
scraped_data <- readRDS("scraped_2020-04_ALL.RDS") %>%
  data.frame() %>%
  mutate(days_to_sell = as.numeric(days_to_sell)) %>%
  rename(posh_user = user)

# Add to database
dbWriteTable(con, name = "solds", value = scraped_data, overwrite = T)
dbGetQuery(con, 'ALTER TABLE solds ADD CONSTRAINT solds_pk PRIMARY KEY ("item_id")')

rm(scraped_data)
gc()


# Append further months
AddMonth <- function(filepath){
  scraped_data <- readRDS(filepath) %>%
    data.frame() %>%
    mutate(days_to_sell = as.numeric(days_to_sell)) %>%
    rename(posh_user = user)
  
  id_query <- dbSendQuery(con, "SELECT item_id FROM solds")
  existing_id <- dbFetch(id_query)
  dbClearResult(id_query)

  remove_id <- intersect(scraped_data$item_id, existing_id$item_id)
  cat(paste0(length(remove_id), " ids were removed as duplicates \n"))
  scraped_data %<>% filter(!(item_id %in% remove_id))
  dbWriteTable(con, name = "solds", value = scraped_data, append = T)

  rm(scraped_data)
  gc()
  return(remove_id)
}



remove5 <- AddMonth("scraped_2020-05_ALL.RDS")
remove6 <- AddMonth("scraped_2020-06_ALL.RDS")
remove7 <- AddMonth("scraped_2020-07_ALL.RDS")
remove8 <- AddMonth("scraped_2020-08_ALL.RDS")
remove9 <- AddMonth("scraped_2020-09_ALL.RDS")

save(remove5, remove6, remove7, remove8, remove9, file = "./sql/removed_ids_5-9.RDa")
