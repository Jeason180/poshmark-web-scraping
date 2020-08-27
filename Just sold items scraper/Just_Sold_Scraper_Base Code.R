library(rvest)
library(dplyr)
library(stringr)
library(magrittr)

source("./code/Posh Scraping Functions.R")

# Get list of input URLs from master code

names(url_inputs) %<>% tolower %>% gsub(" ", "_", .)
url_inputs %<>% select(market, category, subcategory, price_range, page, url)


# Run scraper through URL inputs
ptm <- proc.time()
scrape_urls <-url_inputs$url
scraped_results_list <- lapply(scrape_urls, ScrapeSearchResultsWrap)
proc.time() - ptm

result_classes <- sapply(scraped_results_list, class)
rescrape_round2 <- scrape_urls[result_classes == "character"]
scraped_results_list <- scraped_results_list[result_classes != "character"]
gc()

# Rescrape - any URLs that failed
scraped_results_list2 <- lapply(rescrape_round2, ScrapeSearchResultsWrap)
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

