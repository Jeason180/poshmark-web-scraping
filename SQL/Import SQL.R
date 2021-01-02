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
#dbListTables(con)





### Only run when setting up initial database

# Load initial data
scraped_data <- readRDS("scraped_2020-04_ALL.RDS") %>%
  data.frame() %>%
  mutate(days_to_sell = as.numeric(days_to_sell)) %>%
  rename(posh_user = user)

# change AA_multiple category
scraped_data %<>% mutate(subcategory = if_else(subcategory == "AA_Multiple", 
                                               "Multiple", subcategory))

# Update Pants category to Pants & Jumpsuits
scraped_data %<>% mutate(category = if_else(category == "Pants" & market == "Women", 
                                            "Pants & Jumpsuits", category))

# Update helper categories
scraped_data %<>% mutate(mkt_cat = paste(market, category, sep = "_"),
                         super_category = paste(market, category, subcategory, sep = "_"))

# Add to database
dbWriteTable(con, name = "solds", value = scraped_data, overwrite = T)
dbSendQuery(con, 'ALTER TABLE solds ADD CONSTRAINT solds_pk PRIMARY KEY ("item_id")')

rm(scraped_data)
gc()





### Function to add additional months of data



# Append further months
AddMonth <- function(filepath){
  scraped_data <- readRDS(filepath) %>%
    data.frame() %>%
    mutate(days_to_sell = as.numeric(days_to_sell)) %>%
    rename(posh_user = user)
  
  # Update categories
  scraped_data %<>% mutate(subcategory = if_else(subcategory == "AA_Multiple", 
                                                 "Multiple", subcategory))
  scraped_data %<>% mutate(category    = if_else(category == "Pants" & market == "Women", 
                                                 "Pants & Jumpsuits", category))
  
  # Update helper categories
  scraped_data %<>% mutate(mkt_cat = paste(market, category, sep = "_"),
                           super_category = paste(market, category, subcategory, sep = "_"))
  
  
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

remove10 <- AddMonth("scraped_2020-10_ALL.RDS")
save(remove10, file = "./sql/removed_ids_10.RDa")

remove11 <- AddMonth("scraped_2020-11_ALL.RDS")
save(remove11, file = "./sql/removed_ids_11.RDa")


# one off removals
dbSendQuery(con, "DELETE FROM solds WHERE item_id = '5b03b92b3b1608d5e527040b'") # not a real price, distorts results



# SQL extras
# change AA_Multiple category
#dbSendQuery(con, "UPDATE solds SET subcategory = 'Multiple' WHERE subcategory = 'AA_Multiple'") 

# update Pants category to Pants & Jumpsuits
#dbSendQuery(con, "UPDATE solds SET category = 'Pants & Jumpsuits' WHERE category = 'Pants' AND market = 'Women'") 
#dbSendQuery(con, "UPDATE solds SET mkt_cat = 'Women_Pants & Jumpsuits' WHERE mkt_cat = 'Women_Pants'") 
#dbSendQuery(con, "UPDATE solds SET super_category = CONCAT_WS('_', market, category, subcategory)") 
#dbSendQuery(con, "UPDATE solds SET super_category = CONCAT_WS('_', super_category, 'NA') WHERE subcategory IS NULL") 

#test <- dbGetQuery(con, "SELECT super_category, COUNT(item_id) FROM solds GROUP BY super_category")
