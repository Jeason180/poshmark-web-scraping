# Rescrape price and size information from missing spots
library(dplyr)
library(magrittr)
library(rvest)


setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()

date_sold_start <- "2020-06-29"
date_sold_end   <- "2020-06-29"

# Load data
scraped_data <- readRDS("scraped_2020-06_ALL.RDS")

missing_price_all <- scraped_data %>%
  filter(is.na(price)) %>%
  select(item_id, title, item_url, date_sold, user)

missing_price <- missing_price_all %>%
  filter(date_sold >= date_sold_start & date_sold <= date_sold_end)

missing_price %<>% mutate(item_url = paste0("http://www.poshmark.com/", item_url))


# Nodes to Text Wrapper
NodesToText <- function(page, tag){
  html_data <- html_nodes(page, tag)
  text <- html_text(html_data) %>% 
    as.character() %>% trimws
  if(length(text) == 0) text <- NA
  return(text)
}


# Function to scrape bits on an individual page
ScrapePriceSize <- function(item_url, item_id){
  i <<- i+1

  if(floor(i/10) == i/10) cat(paste("Scraping item", i,"\n"))
  
  now <- Sys.time()
  webpage <- try(read_html(item_url))
  if(unique(class(webpage) == "try-error")){
    cat(paste("Item", i, "URL was not accessible, returning NULL \n"))
    results <- data.frame(item_id = item_id,
                          title = NA,
                          price = NA,
                          size = NA,
                          item_url = item_url,
                          date_scraped = now,
                          stringsAsFactors = F)
    
    q <- runif(1, min = 0.1, max = 1)
    Sys.sleep(1+q)
    
    rm(webpage)
    gc()
    return(results)
  }
  
  # Grabbing price and size info
  price <- NodesToText(webpage, ".listing__ipad-centered h1")
  price <- strsplit(price, "\n")[[1]][1] %>% gsub("$", "", ., fixed = T) %>% 
    gsub(",", "", ., fixed = T) %>% as.numeric
  
  size <- NodesToText(webpage, ".listing__size-selector-con")
  title <- NodesToText(webpage, ".listing__title .fw--light")
  
  results <- data.frame(item_id = item_id,
                        title = title,
                        price = price,
                        size = size,
                        item_url = item_url,
                        date_scraped = now,
                        stringsAsFactors = F)
  
  if(nrow(results) !=1) cat(paste("Warning: item", title, "does not have 1 entry \n"))
  
  q <- runif(1, min = 0.1, max = 1)
  #cat(paste("sleeping", 3+q, "seconds \n"))
  Sys.sleep(1+q)
  
  return(results)
  
}


# Actually scrape it
# Number of obs to start with

num <- nrow(missing_price)

start_time <- Sys.time()
i <- 0
results <- mapply(ScrapePriceSize, missing_price$item_url[1:num], missing_price$item_id[1:num], SIMPLIFY = FALSE) %>% bind_rows
end_time <- Sys.time()

end_time - start_time

# Redoing the ones that didn't work the first time
leftover <- results %>% filter(is.na(price))
results2 <- mapply(ScrapePriceSize, leftover$item_url, leftover$item_id, SIMPLIFY = FALSE) %>% bind_rows
full_results <- bind_rows(results, results2) %>% filter(!is.na(price)) %>% select(-title)


# Join back with the info before
price_results <- left_join(missing_price, full_results, by = c("item_id", "item_url")) %>% filter(!is.na(price))

save(price_results, file ="./price rescrape files/prices_2020-06-29.RDa")


