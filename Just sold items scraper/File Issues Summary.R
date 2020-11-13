# Summarize where data is missing
library(tidyverse)
library(magrittr)



setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()

# Read file names from disk
filenames <- list.files("raw files", recursive = T) %>% tibble(full_name = .)

# Extract month and key features
filenames %<>% 
  separate(full_name, into = c("month", "file"), sep = "/") %>%
  mutate(date = str_extract(file, "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"),
         subcategory = str_detect(file, "subcategory"))

# Count by date
file_summary <- filenames %>%
  group_by(month, date, subcategory) %>%
  summarize(count = n())

# pivot wider
file_summary %<>% 
  mutate(subcategory = if_else(subcategory, "subcategory_count", "general_count")) %>%
  pivot_wider(names_from = subcategory, values_from = count, values_fill = 0)

# identify dates with problems
file_summary %<>% 
  mutate(problem = if_else(general_count != 48 | subcategory_count != 24, 1, 0))

# monthly summary
month_summary <- file_summary %>%
  group_by(month) %>%
  summarize(count_problems = sum(problem))



