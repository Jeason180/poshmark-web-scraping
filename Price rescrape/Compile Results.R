library(tidyverse)
library(magrittr)


setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()

month_of_interest <- "2020-07"


# Load data
scraped_items <- readRDS(paste0("./scraped_", month_of_interest, "_ALL.RDS"))

rescrape_files <- list.files("./price rescrape files") %>% 
  grep(month_of_interest, ., value = T, fixed = T) %>% paste0("./price rescrape files/", .)


# Function to load individual price rescrape files
load_files <- function(file){
  load(file)
  return(price_results)
}

rescrape_data <- lapply(rescrape_files, load_files) %>% bind_rows

