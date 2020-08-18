# Compile and Dedupe Just Solds
library(dplyr)
library(magrittr)
library(writexl)
library(filesstrings)

setwd("C:/Users/maran/Dropbox/Web Scraping")
rm(list = ls())
gc()

# Read in all files
files_to_read <- list.files("./inter/just_sold") %>% setdiff(c("logs", "compiled_datasets", "scraped_solds_compiled.RDS", "scraped_solds_compiled - Copy.RDS"))
files_to_read <- paste0("./inter/just_sold/", files_to_read)

compiled_data <- readRDS("./inter/just_sold/scraped_solds_compiled.RDS")
#files <- lapply(files_to_read, function(x) results <- readRDS(x))

scraped_items <-lapply(files_to_read, function(x) results <- readRDS(x)) %>% bind_rows
length(unique(scraped_items$item_id))



#### Checking items that are found in multiple categories
category_items <- scraped_items %>% filter(!is.na(subcategory))
category_items %<>% arrange(market, category, item_id, subcategory, date_sold, price, scrape_time) %>% 
  distinct(item_id, subcategory, .keep_all = T)

item_check <- category_items %>% group_by(item_id) %>% count()
items_multiple <- category_items %>% filter(item_id %in% item_check$item_id[item_check$n > 1])

#length(unique(items_multiple$item_id)) /length(unique(category_items$item_id)) 
# 7% of records seem to appear in multiple categories.


items_multiple_touse <- items_multiple %>% 
  select(item_id, subcategory) %>%
  rename(subcategory_original = subcategory)

# items_multiple_collapsed <- scraped_items %>%
#   filter(item_id %in% items_multiple$item_id & !is.na(subcategory)) %>%
#   arrange(item_id, scrape_time) %>%
#   filter(subcategory != "AA_Multiple") %>%
#   group_by(item_id, subcategory) %>%
#   summarize(count = n())

items_multiple_collapsed <- items_multiple_touse %>% 
  group_by(item_id) %>% 
  mutate(subcategory_all = paste(subcategory_original, collapse = " ")) %>%
  select(-subcategory_original) %>% 
  distinct

items_multiple <- merge(items_multiple, items_multiple_collapsed, all = T, by = "item_id")
items_multiple$subcategory <- "AA_Multiple"

scraped_items <- bind_rows(scraped_items, items_multiple, compiled_data)
rm(item_check, items_multiple_collapsed, items_multiple_touse)





# Dedupe
scraped_items %<>% arrange(market, category, date_sold, price, item_id, subcategory, scrape_time) %>% 
  mutate(mkt_cat = paste0(market, "_", category)) %>%
  distinct(item_id, .keep_all = T)
gc()





# Save Output
saveRDS(scraped_items, file = "inter/just_sold/scraped_solds_compiled.RDS")
save(scraped_items,file = "output/just sold/datasets/scraped_solds.RDa")


scraped_items_output <- scraped_items %>% select(-item_url, -search_url)
write_xlsx(scraped_items_output, paste0("output/just sold/scraped_solds", ".xlsx"))

# Move compiled datasets into new folder
file.move(files_to_read, "./inter/just_sold/compiled_datasets")

         
# # Word analysis
# rownum <- 1
# data <- scraped_items
# stop_words <- c("nwt", "size", "men", "mens", "sz", "the")
# 
# 
# SplitWords <- function(string){
#   string <- gsub("[[:punct:]]", " ", string)
#   words <- strsplit(string, " ") %>% unlist
#   words %<>% trimws %>% tolower
#   words <- setdiff(words, "")
#   return(words)
# }
# 
# WordAnalysis <- function(rownum, stop_words){
#   
#   if(floor(rownum/1000) == rownum/1000) cat(paste("row is", rownum, "\n"))
#   
#   title <- data$title[rownum]
#   item_id <- data$item_id[rownum]
#   market <- data$market[rownum]
#   category <- data$category[rownum]
#   price <- data$price[rownum]
#   brand <- data$brand[rownum]
#   
#   title_words <- SplitWords(title)
#   brand_words <- SplitWords(brand)
#   category_words <- SplitWords(category)
#   
#   total_stop_words <- c(stop_words, brand_words, category_words)
#   
#   words <- setdiff(title_words, total_stop_words)
#   words <- words[nchar(words) > 1]
#   
#   if(length(words) == 0){
#     return(NULL)
#     next
#   }
#   
#   words_df <- data.frame(word = words, stringsAsFactors = F)
#   words_df$item_id <- item_id
#   words_df$market <- market
#   words_df$category <- category
#   words_df$price <- price
#   words_df$brand <- brand
#   
#   return(words_df)
#   
# }
# 
# scraped_words <- lapply(1:nrow(scraped_items), WordAnalysis, stop_words = stop_words) %>% bind_rows
# write.csv(scraped_words, "output/just sold/scraped words.csv", row.names = F)



