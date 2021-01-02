# Rescrape price and size information from missing spots
library(dplyr)
library(magrittr)
library(rvest)


setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()


scraped_data <- readRDS("scraped_2020-11_ALL.RDS")

#date <- "2020-06-26"

date_sold_start <- "2020-11-01"
date_sold_end <- "2020-11-30"


missing_subcategory_all <- scraped_data %>%
  filter(is.na(subcategory)) %>%
  select(market, category, item_id, title, item_url, date_sold, user) %>%
  filter(market == "Women" & category %in% c("Pants", "Pants & Jumpsuits", "Tops", "Jeans", "Dresses", "Shoes"))

missing_subcategory <- missing_subcategory_all %>%
  filter(date_sold >= date_sold_start & date_sold <= date_sold_end)

missing_subcategory %<>% mutate(item_url = paste0("http://www.poshmark.com", item_url))




# Nodes to Text Wrapper
NodesToText <- function(page, tag) {
  html_data <- html_nodes(page, tag)
  text <- html_text(html_data) %>%
    as.character() %>%
    trimws()
  if (length(text) == 0) text <- NA
  return(text)
}

# Function to scrape bits on an individual page
ScrapeSubcategory <- function(item_url, item_id) {
  i <<- i + 1
  
  if (floor(i / 10) == i / 10) cat(paste("Scraping item", i, "\n"))
  
  now <- Sys.time()
  webpage <- try(read_html(item_url))
  if (unique(class(webpage) == "try-error")) {
    cat(paste("Item", i, "URL was not accessible, returning NULL \n"))
    results <- data.frame(
      item_id = item_id,
      title = NA,
      market = NA,
      category = NA,
      subcategory = NA,
      item_url = item_url,
      date_scraped = now,
      fail_reason = "URL Read",
      stringsAsFactors = F
    )
    
    q <- runif(1, min = 0.1, max = 1)
    Sys.sleep(1 + q)
    
    rm(webpage)
    gc()
    return(results)
  }
  
  # Grabbing subcategory info
  market <- NodesToText(webpage, "a[data-et-name*='listing_details_category']")[1]
  category <- NodesToText(webpage, "a[data-et-name*='listing_details_category']")[2]
  subcategory <- NodesToText(webpage, paste0("a[data-et-name*='listing_details_category'] + .btn"))
  subcategory <- ifelse(length(subcategory == 2), subcategory[2], NA)
  subcategory <- gsub(" ", "_", subcategory)
  title <- NodesToText(webpage, ".listing__title .fw--light")
  
  # If category is NA, move on
  if (is.na(category)) {
    cat(paste("Item", i, "category is NA, returning NULL \n"))
    results <- data.frame(
      item_id = item_id,
      title = NA,
      market = NA,
      category = NA,
      subcategory = NA,
      item_url = item_url,
      date_scraped = now,
      fail_reason = "Category NA",
      stringsAsFactors = F
    )
    
    q <- runif(1, min = 0.1, max = 1)
    Sys.sleep(1 + q)
    
    rm(webpage)
    gc()
    return(results)
  }

  
  results <- data.frame(
    item_id = item_id,
    title = title,
    market = market,
    category = category,
    subcategory = subcategory,
    item_url = item_url,
    date_scraped = now,
    fail_reason = "success",
    stringsAsFactors = F
  )
  
  if (nrow(results) != 1) cat(paste("Warning: item", title, "does not have 1 entry \n"))
  
  q <- runif(1, min = 0.1, max = 1)
  Sys.sleep(1 + q)
  
  return(results)
}


# Actually scrape it
num <- nrow(missing_subcategory)

start_time <- Sys.time()
i <- 0
results <- mapply(ScrapeSubcategory, missing_subcategory$item_url[1:num], missing_subcategory$item_id[1:num], SIMPLIFY = FALSE) %>% 
  bind_rows()
end_time <- Sys.time()

end_time - start_time

# Redoing for ones that didn't work
leftover <- results %>% filter(fail_reason != "success")
results2 <- mapply(ScrapeSubcategory, leftover$item_url, leftover$item_id, SIMPLIFY = FALSE) %>% bind_rows()

full_results <- bind_rows(results, results2) %>%
  filter(fail_reason == "success") %>%
  select(-title, -market, -category)


# Join back with the info before
subcategory_results <- left_join(missing_subcategory, full_results, by = c("item_id", "item_url"))

 
save(subcategory_results, file = paste0("./price rescrape files/subcategory_2020-11_rescrape.RDa"))



