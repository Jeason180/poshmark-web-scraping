# Compile and Dedupe Just Solds
library(dplyr)
library(magrittr)
library(writexl)
library(filesstrings)
library(tidyr)
library(lubridate)

setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()

# Read in all files
files_to_read <- list.files("raw files/2021-01")
files_to_read <- paste0("./raw files/2021-01/", files_to_read)

scraped_items <-lapply(files_to_read, function(x) results <- readRDS(x)) %>% bind_rows


old_items <- readRDS("./compiled monthly/scraped_2020-12_ALL.RDS")
gc()


# For June/July 2020 - remove all subcategories observations under category "Pants" 
# due to name change in subcategory to "Pants & Jumpsuits"
# scraped_items <- scraped_items %>%
#   filter(!(category == "Pants" & 
#          !is.na(subcategory) & 
#          date_sold >= "2020-06-04" & 
#          date_sold <= "2020-07-25"))
# gc()



Dedupe_Merge <- function(new_items, compiled_items = NULL){
  
  # Subset of all new items that have category information
  category_items <- new_items %>% filter(!is.na(subcategory))
  category_items %<>% arrange(market, category, item_id, subcategory, date_sold, price, scrape_time) %>% 
    group_by(item_id, category, subcategory) %>% 
    count()
  gc()
  
  # If we have old items to compare against...
  if(!is.null(compiled_items)){
    
    # Transform old data into version to merge with new data
    new_cat_ids <- category_items %>% pull(item_id) %>% unique
    category_items_old_full <- compiled_items %>% filter(!is.na(subcategory)) %>% filter(item_id %in% new_cat_ids)
    category_items_old <- category_items_old_full %>% select(item_id, category, categories_voted, votes_received)
    
    category_items_old <- separate(category_items_old, categories_voted, 
                                   into = c("cat1", "cat2", "cat3", "cat4", "cat5", "cat6", "cat7","cat8"),
                                   sep = " ", fill = "right")
    category_items_old <- separate(category_items_old, votes_received, 
                                   into = c("vote1", "vote2", "vote3", "vote4", "vote5", "vote6", "vote7", "vote8"),
                                   sep = " ", fill = "right")
    
    category_items_old %<>% unite(total_vote1, cat1, vote1, sep = " ")
    category_items_old %<>% unite(total_vote2, cat2, vote2, sep = " ")
    category_items_old %<>% unite(total_vote3, cat3, vote3, sep = " ")
    category_items_old %<>% unite(total_vote4, cat4, vote4, sep = " ")
    category_items_old %<>% unite(total_vote5, cat5, vote5, sep = " ")
    category_items_old %<>% unite(total_vote6, cat6, vote6, sep = " ")
    category_items_old %<>% unite(total_vote7, cat7, vote7, sep = " ")
    category_items_old %<>% unite(total_vote8, cat8, vote8, sep = " ")
    category_items_old %<>% rename(full_category = category) %>%
      select(-starts_with("cat"), -starts_with("vote"))
    
    category_items_old <- pivot_longer(category_items_old, cols = starts_with("total_vote"), names_to = "catnum", values_to = "catvote")
    
    category_items_old %<>% filter(catvote != "NA NA") %>% select(-catnum)
    category_items_old %<>% separate(catvote, into = c("subcategory", "n"), sep = " ")
    category_items_old %<>% rename(category = full_category) 
    category_items_old %<>% mutate(n = as.numeric(n))
    
    rm(category_items_old_full)
    
    # Bind with new data and collapse votes by subcategory
    category_items <- bind_rows(category_items, category_items_old) %>%
      arrange(item_id, category, subcategory) %>% 
      group_by(item_id, category, subcategory) %>%
      summarize(n = sum(n))
    
    rm(category_items_old)
    gc()
  }
  
  
 
  # Get list of items that appear in multiple categories
  item_check <- category_items %>% group_by(item_id) %>% count()
  multiple_categories <- item_check %>% filter(n > 1) %>% pull(item_id)
  #category_items <- category_items %>% filter(item_id %in% multiple_categories)
  
  
  # Rank each item by number of "votes" it received in prior categories
  category_items %<>% 
    group_by(item_id) %>% 
    mutate(rank = rank(-n, ties.method = "min")) %>%
    arrange(item_id, rank)
  
  category_items  %<>%
    group_by(item_id) %>%
    arrange(item_id, subcategory) %>%
    mutate(categories_voted = paste(subcategory, collapse = " "),
           votes_received = paste(n, collapse = " "))
  
  reduced_results <- category_items %>%
    filter(rank == 1)
  
  # Vector of item IDs that have ties
  ties <- reduced_results %>% 
    group_by(item_id) %>% 
    summarize(count = n()) %>%
    filter(count > 1) %>% 
    pull(item_id)
  
  # Create new column called subcategory_final with final determined category
  reduced_results %<>% mutate(tie = item_id %in% ties) %>%
    mutate(subcategory_final = ifelse(tie == TRUE, "AA_Multiple", subcategory)) %>%
    mutate(subcategory_final = ifelse((categories_voted == "Tees__Long_Sleeve Tees__Short_Sleeve" & tie == T),
                                      "Tees", subcategory_final))
  
  final_results <- reduced_results %>% select(-subcategory) %>%
    distinct()

  if(!length(unique(reduced_results$item_id)) == length(unique(final_results$item_id))){
    cat(paste("Warning: some items still have multiple categories \n"))
  }
  
  # Final data frame that has the results for each item: what subcategories it was "voted" as, how many votes each received,
  # and what the final category was determined as
  final_results %<>% select(-tie, -n, -rank) %>% rename(subcategory = subcategory_final)
  
  # Data frame of all items, new and old
  all_items <- bind_rows(new_items, compiled_items)
  
  # Full item information for all items that we have category information
  full_info <- all_items %>% filter(item_id %in% final_results$item_id) %>% select(-subcategory)
  if("categories_voted" %in% names(full_info)) full_info %<>% select(-categories_voted, -votes_received)
  
  # Merge the full item info with the determined subcategories
  full_info <- inner_join(full_info, final_results, by = c("item_id", "category")) %>% arrange(item_id, scrape_time)
  
  # Remaining items without subcategory information
  other_items <- all_items %>% filter(!(item_id %in% final_results$item_id))
  
  
  # Compile all results back together and dedupe them
  final_data <- bind_rows(other_items, full_info) %>% 
    arrange(market, category, date_sold, price, item_id, subcategory, scrape_time, categories_voted)
  
  final_data_deduped <- final_data %>%
    distinct(item_id, .keep_all = T)
  gc()
  
  na_items <- final_data_deduped$item_id[is.na(final_data_deduped$subcategory)]
  if(sum(na_items %in% category_items$item_id) != 0) cat("Warning: some items have NA category despite having other category information")
  
  # Create 'super category' item marker
  final_data_deduped %<>% 
    mutate(super_category = paste(market, category, subcategory, sep = "_"), 
           mkt_cat = paste0(market, "_", category))  %>%
    select(super_category, mkt_cat, everything()) %>%
    arrange(market, category, date_sold, subcategory, price, scrape_time)
  
  
  return(final_data_deduped)
  
  }


results <- Dedupe_Merge(scraped_items, compiled_items = old_items)
gc()

results_crop <- results %>% filter(date_sold >= "2021-01-01" & date_sold <= "2021-01-18")

#saveRDS(results_crop, "./compiled monthly/scraped_2020-12_ALL.RDS")
saveRDS(results_crop, "./compiled partial/scraped_2021-01_1-18.RDS")



#scraped_items_output <- results %>% select(-item_url, -search_url)
#write.csv(scraped_items_output, "C:/Users/maran/Dropbox/Web Scraping/output/just sold/scraped_solds.csv", row.names = F)
