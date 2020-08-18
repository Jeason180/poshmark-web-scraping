library(rvest)
library(dplyr)
library(stringr)
library(readxl)
library(magrittr)

rm(list = ls())
gc()

#Sys.setenv(TZ='EDT')

#setwd("C:/Users/maran/Dropbox/Web Scraping")
setwd("C:/Users/Administrator/Dropbox/Web_Scraping")
source("./code/Posh Scraping Functions.R")

# Read in list of input URLs
url_inputs <- read_excel("./input/Just Sold URLs.xlsx", sheet = "All")
names(url_inputs) %<>% tolower %>% gsub(" ", "_", .)
url_inputs %<>% select(market, category, subcategory, price_range, page, url)


# Run scraper through URL inputs
ptm <- proc.time()
scrape_urls <-url_inputs$url
scraped_results_list <- lapply(scrape_urls, ScrapeSearchResultsWrap_Modified)
proc.time() - ptm

result_classes <- sapply(scraped_results_list, class)
rescrape_round2 <- scrape_urls[result_classes == "character"]
scraped_results_list <- scraped_results_list[result_classes != "character"]
gc()

# Rescrape - any URLs that failed
scraped_results_list2 <- lapply(rescrape_round2, ScrapeSearchResultsWrap_Modified)
result_classes2 <- sapply(scraped_results_list2, class)
scraped_results_list2 <- scraped_results_list2[result_classes2 != "character"]

proc.time() - ptm
gc()

# Combine results
scraped_results <- bind_rows(scraped_results_list, scraped_results_list2)

# Add back page info, calculate some date and category fields
scraped_results <- right_join(url_inputs, scraped_results, by = c("url" = "search_url")) %>% 
  select(-page) %>%
  mutate(date_sold = as.Date(scrape_time, tz = "America/New_York")) %>%
  mutate(days_to_sell = date_sold - date_posted,
         super_category = paste(market, category, subcategory, sep = "_")) %>%
  select(super_category, everything()) %>%
  rename(search_url = url) %>%
  select(-search_url, -item_url, -item_id, item_id, item_url, search_url)

# Add ID info for scrape
scrape_time <- min(scraped_results$scrape_time)
date <- as.Date(scrape_time, tz = "America/New_York")
time <- strftime(scrape_time, "%H%M", tz = "America/New_York")
saveid <- paste0(date, "_", time)

scraped_results$scrape_id <- saveid

saveRDS(scraped_results, file = paste0("./inter/just_sold/results_", saveid,".RDS"))



                              