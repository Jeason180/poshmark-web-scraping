library(tidyverse)
library(magrittr)


setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()

month_of_interest <- "2020-05"


# Load data
scraped_items <- readRDS(paste0("./scraped_", month_of_interest, "_ALL.RDS"))

rescrape_files <- list.files("./price rescrape files") %>%
  grep(month_of_interest, ., value = T, fixed = T) %>%
  paste0("./price rescrape files/", .)


# Function to load individual price rescrape files
load_files <- function(file) {
  load(file)
  return(price_results)
}

rescrape_data <- lapply(rescrape_files, load_files) %>% bind_rows()


# Split into items that need update and ones that don't
okay_items <- scraped_items %>% filter(!(item_id %in% rescrape_data$item_id))
update_items <- scraped_items %>% filter((item_id %in% rescrape_data$item_id))

# Merge rescraped info and orignal info
rescrape_formerge <- rescrape_data %>%
  select(item_id, price, size, user) %>%
  rename(
    user_check = user,
    price_update = price,
    size_update = size
  )

update_items %<>% left_join(rescrape_formerge, by = "item_id")

# Update price and size
update_items %<>% mutate(
  price = if_else(is.na(price), price_update, price),
  size = if_else(is.na(size), size_update, size),
  check = if_else(user == user_check, TRUE, FALSE)
)

table(update_items$check, useNA = "always")

# clean up extra columns
update_items %<>% select(-price_update, -size_update, -user_check, -check)

# add back updated and non-updated items
final_items <- bind_rows(okay_items, update_items) %>% 
  arrange(date_sold, super_category, scrape_time)


# Save
saveRDS(final_items, paste0("./scraped_", month_of_interest, "_ALL.RDS"))
