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

url_inputs <- read_excel("./input/Just Sold URLs Subcategories.xlsx", sheet = "Hourly")
names(url_inputs) %<>% tolower %>% gsub(" ", "_", .)
url_inputs %<>% select(market, category, subcategory, page, price_range, url)


ptm <- proc.time()
scrape_urls <-url_inputs$url
scraped_results_list <- lapply(scrape_urls, ScrapeSearchResultsWrap_Modified)
proc.time() - ptm

result_classes <- sapply(scraped_results_list, class)
rescrape_round2 <- scrape_urls[result_classes == "character"]
scraped_results_list <- scraped_results_list[result_classes != "character"]
gc()

# Rescrape - 2
scraped_results_list2 <- lapply(rescrape_round2, ScrapeSearchResultsWrap_Modified)
result_classes2 <- sapply(scraped_results_list2, class)
rescrape_round3 <- rescrape_round2[result_classes2 == "character"]
scraped_results_list2 <- scraped_results_list2[result_classes2 != "character"]

# Rescrape - 3
scraped_results_list3 <- lapply(rescrape_round3, ScrapeSearchResultsWrap_Modified)
result_classes3 <- sapply(scraped_results_list3, class)
rescrape_round4 <- rescrape_round3[result_classes3 == "character"]
scraped_results_list3 <- scraped_results_list3[result_classes3 != "character"]

# Rescrape - 4
scraped_results_list4 <- lapply(rescrape_round4, ScrapeSearchResultsWrap_Modified)
result_classes4 <- sapply(scraped_results_list4, class)
rescrape_round5 <- rescrape_round4[result_classes4 == "character"]
scraped_results_list4 <- scraped_results_list4[result_classes4 != "character"]

# Rescrape - 5
scraped_results_list5 <- lapply(rescrape_round5, ScrapeSearchResultsWrap_Modified)
result_classes5 <- sapply(scraped_results_list5, class)
scraped_results_list5 <- scraped_results_list5[result_classes5 != "character"]
result_classes5

proc.time() - ptm
gc()

scraped_results <- bind_rows(scraped_results_list, scraped_results_list2,
                             scraped_results_list3, scraped_results_list4,
                             scraped_results_list5)

scraped_results <- right_join(url_inputs, scraped_results, by = c("url" = "search_url")) %>% 
  select(-page) %>%
  mutate(date_sold = as.Date(scrape_time, tz = "America/New_York")) %>%
  mutate(days_to_sell = date_sold - date_posted,
         super_category = paste(market, category, subcategory, sep = "_")) %>%
  select(super_category, everything()) %>%
  rename(search_url = url) %>%
  select(-search_url, -item_url, -item_id, item_id, item_url, search_url)

scrape_time <- min(scraped_results$scrape_time)
date <- as.Date(scrape_time, tz = "America/New_York")
time <- strftime(scrape_time, "%H%M", tz = "America/New_York")
saveid <- paste0(date, "_", time)

scraped_results$scrape_id <- saveid

saveRDS(scraped_results, file = paste0("./inter/just_sold/results_subcategory_", saveid,".RDS"))



