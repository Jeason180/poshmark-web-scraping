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

# helper function to get query
myGet <- function(query){
  solds_query <- dbSendQuery(con, query)
  results <- dbFetch(solds_query)
  dbClearResult(solds_query)
  return(results)
}

# total sales over time
sales <- myGet("SELECT date_sold, market, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market")


# by category
category_sales <- myGet("SELECT date_sold, market, category, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market, category")


