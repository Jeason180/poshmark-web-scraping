library(tidyverse)
library(magrittr)


setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()

month_of_interest <- "2020-11"


# Load data
scraped_items <- readRDS(paste0("./scraped_", month_of_interest, "_ALL.RDS"))


#rescrape_files <- c("subcategory_20200604_20200624.RDa", "subcategory_20200625_20200630.RDa") %>%
#  paste0("./price rescrape files/", .)

rescrape_files <- c("subcategory_2020-11_rescrape.RDa") %>%
  paste0("./price rescrape files/", .)


# Function to load individual price rescrape files
load_files <- function(file) {
  load(file)
  return(subcategory_results)
}

rescrape_data <- lapply(rescrape_files, load_files) %>% bind_rows() %>% filter(!is.na(subcategory))


# Standardize subcategory names
rescrape_data$subcategory <- gsub("Tees_-_", "Tees__", rescrape_data$subcategory, fixed = T)


# Split into items that need update and ones that don't
okay_items <- scraped_items %>% filter(!(item_id %in% rescrape_data$item_id))
update_items <- scraped_items %>% filter((item_id %in% rescrape_data$item_id))

# Merge rescraped info and orignal info
rescrape_formerge <- rescrape_data %>%
  select(item_id, subcategory, user) %>%
  rename(
    user_check = user,
    subcategory_update = subcategory
  )

update_items %<>% left_join(rescrape_formerge, by = "item_id")

# Update subcategory
update_items %<>% mutate(
  subcategory = if_else(is.na(subcategory), subcategory_update, subcategory),
  check = if_else(user == user_check, TRUE, FALSE)
)

table(update_items$check, useNA = "always")

# clean up extra columns
update_items %<>% select(-subcategory_update, -user_check, -check)

# add back updated and non-updated items
final_items <- bind_rows(okay_items, update_items) %>% 
  arrange(date_sold, super_category, scrape_time)


# Save
saveRDS(final_items, paste0("./scraped_", month_of_interest, "_ALL.RDS"))
