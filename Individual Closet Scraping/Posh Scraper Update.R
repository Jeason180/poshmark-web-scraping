library(rvest)
library(dplyr)
library(stringr)
library(magrittr)
library(lubridate)

rm(list = ls())
setwd("C:/Users/maran/Dropbox/Web Scraping")
source("./code/Posh Scraping Functions.R")

links_path <- "./input/individual closets/emptyhanger_20200724.html"

# Scraping search results to get links
closet_links <- ScrapeSearchResultsWrap(links_path)

save(closet_links, file = "./inter/individual closets/emptyhanger_closet_links_20200724.RDa")


# Compare against already scraped links
load("./inter/individual closets/emptyhanger_closet_links_20200724.RDa")
closet_links_new <- closet_links
load("./inter/individual closets/emptyhanger_closet_links_20191215.RDa")
closet_links <- emptyhanger_closet_links

#deleted_items <- closet_links %>% filter(status == "Available") %>% filter(!(item_id %in% closet_links_new$item_id))

closet_links %<>% mutate(match_id = paste0(item_id, "_", status))
closet_links_new %<>% mutate(match_id = paste0(item_id, "_", status))

# Scrape all new items and items with a status change since last scrape
# Then, remove all previously scraped listings that have since been deleted

items_in_common <- intersect(closet_links$match_id, closet_links_new$match_id)
links_to_scrape <- closet_links_new %>% 
  filter(!(match_id %in% items_in_common)) %>% 
  pull(item_url)

ptm <- proc.time()
update_closet <- ScrapeItemWrap(links_to_scrape)
ptm - proc.time()

save(update_closet, file = "./inter/individual closets/emptyhanger_closet_update_20200724.RDa")


# Merge and update

load("./output/closets/closets_scraped_201912.RDa")
data_old <- all_closets %>% filter(user == "emptyhanger")

load("./inter/individual closets/emptyhanger_closet_update_20200724.RDa")
data_new <- update_closet


# Clean up dates
data_new$date_sold <- sapply(data_new$item_id, DateSold, closet = data_new) %>%
  as.Date(origin = "1970-01-01")

data_new    %<>% mutate(days_to_sell = as.numeric(date_sold - date_posted),
                        month_sold = floor_date(date_sold, "month"),
                        year_sold = year(date_sold),
                        month_posted = floor_date(date_posted, "month"))



updated_ids <- intersect(data_new$item_id, data_old$item_id)

# Combine datasets
combined_data <- data_old %>% 
  filter(!(item_id %in% updated_ids)) %>%
  bind_rows(data_new) %>%
  arrange(status, desc(date_sold), desc(date_posted))

combined_data %<>% mutate(category = ifelse(category == "Pants", "Pants & Jumpsuits", category))


save(combined_data, file = "./output/closets/mogi_closet_20200721.RDa")
