library(rvest)
library(tidyverse)
library(magrittr)
library(lubridate)

rm(list = ls())
setwd("C:/Users/maran/Dropbox/Web Scraping")
source("./code/Posh Scraping Functions.R")


# Update the handle of each closet you want to scrape.
# Specify dates for new update and old update you are comparing against.
# All other file paths update automatically, given folder structure
user <- "maranna_yo"
new_date <- "20201113"
old_date <- "20200904"

new_handle <- paste(user, new_date, sep = "_")
old_handle <- paste(user, old_date, sep = "_")


# new saved HTML path
input_path <- paste0("./input/individual closets/", user, "/closet_", new_handle, ".html")


# Scraping new HTML to get new links
closet_links <- ScrapeSearchResultsWrap(input_path)
save(closet_links, file = paste0("./inter/individual closets/", user, "/closet_links_", new_handle, ".RDa"))



# Compare against already scraped items
load(paste0("./inter/individual closets/", user, "/closet_links_", new_handle, ".RDa"))
closet_links_new <- closet_links
load(paste0("./output/closets/closet_", old_handle, ".RDa"))


deleted_items <- closet %>% 
 filter(status == "Available") %>% 
 filter(!(item_id %in% closet_links_new$item_id))

closet %<>% mutate(match_id = paste0(item_id, "_", status))
closet_links_new %<>% mutate(match_id = paste0(item_id, "_", status))


# Scrape all new items and items with a status change since last scrape
# Then, remove all previously scraped listings that have since been deleted
items_in_common <- intersect(closet$match_id, closet_links_new$match_id)
links_to_scrape <- closet_links_new %>% 
  filter(!(match_id %in% items_in_common)) %>% 
  pull(item_url)

start_time <- Sys.time()
update_closet <- ScrapeItemWrap(links_to_scrape)
end_time <- Sys.time()
end_time - start_time

save(update_closet, deleted_items, file = paste0("./inter/individual closets/", user, "/closet_items_update_", new_handle, ".RDa"))


# Merge and update

load(paste0("./output/closets/closet_", old_handle, ".RDa"))
load(paste0("./inter/individual closets/", user, "/closet_items_update_", new_handle, ".RDa"))
data_new <- update_closet


# Clean up dates
data_new$date_sold <- sapply(data_new$item_id, DateSold, closet = data_new) %>%
  as.Date(origin = "1970-01-01")

data_new    %<>% mutate(days_to_sell = as.numeric(date_sold - date_posted),
                        month_sold = floor_date(date_sold, "month"),
                        year_sold = year(date_sold),
                        month_posted = floor_date(date_posted, "month"))



updated_ids <- intersect(data_new$item_id, closet$item_id)

# Combine datasets
combined_data <- closet %>% 
  filter(!(item_id %in% updated_ids)) %>%
  bind_rows(data_new) %>%
  filter(!item_id %in% deleted_items$item_id) %>%
  arrange(status, desc(date_sold), desc(date_posted))

combined_data %<>% mutate(category = ifelse(category == "Pants" & market == "Women", "Pants & Jumpsuits", category),
                          category = ifelse(category == "Pants & Jumpsuits" & market == "Men", "Pants", category))

closet <- combined_data
save(closet, file = paste0("./output/closets/closet_", new_handle, ".RDa"))

# Move old updated closet into old folder
file.copy(from = paste0("./output/closets/closet_", old_handle, ".RDa"), 
          to = paste0("./output/closets/old_versions/closet_", old_handle, ".RDa"))

file.remove(paste0("./output/closets/closet_", old_handle, ".RDa"))
