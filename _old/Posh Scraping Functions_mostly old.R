# Posh Closet Scraping Functions



# Nodes to Text Wrapper
NodesToText <- function(page, tag){
  html_data <- html_nodes(page, tag)
  text <- html_text(html_data) %>% 
    as.character() %>% trimws
  text <- text %>% ifelse(length(.) == 0, NA, .)
  return(text)
}

#item_url <- links_to_scrape[1]

# Function to scrape an individual page
ScrapeItem <- function(item_url){
  i <<- i+1
  cat(paste(i, "\n"))
  if(floor(i/10) == i/10) cat(paste("Scraping item", i,"\n"))
  
  now <- Sys.time()
  webpage <- try(read_html(item_url))
  if(unique(class(webpage) == "try-error")){
    cat(paste("Item", i, "URL was not accessible, reutrning NULL \n"))
    return(NULL)
  }
  
  # Grabbing key information
  title <- NodesToText(webpage, ".listing__title .fw--light")
  brand <- NodesToText(webpage, ".listing__brand")
  price <- NodesToText(webpage, ".listing__ipad-centered h1 :first-child")
  size <- NodesToText(webpage, ".details .size-box") %>% ifelse(length(.) == 0, NA, .)
  description <- NodesToText(webpage, ".description") %>% ifelse(length(.) == 0, NA, .)
  market <- NodesToText(webpage, ".tag-list a[data-pa-name*='category']:first-child") %>% ifelse(length(.) == 0, NA, .)
  category <- NodesToText(webpage, ".tag-list a[data-pa-name*='category']:nth-child(2)") %>% ifelse(length(.) == 0, NA, .)
  subcategory <- NodesToText(webpage, ".tag-list a:nth-child(3)") %>% ifelse(length(.) == 0, NA, .)
  color1 <- NodesToText(webpage, ".tag-list a[data-pa-name*='color']:first-child") %>% ifelse(length(.) == 0, NA, .)
  color2 <- NodesToText(webpage, ".tag-list a[data-pa-name*='color']:nth-child(2)") %>% ifelse(length(.) == 0, NA, .)
  user <- NodesToText(webpage, ".creator-details .handle")[1] %>% ifelse(length(.) == 0, NA, .)
  last_updated <- NodesToText(webpage, ".creator-details .time")[1] %>% ifelse(length(.) == 0, NA, .)
  
  soldnode <- html_nodes(webpage, ".sold .condition-desc-con .text-con")
  soldoutnode <- html_nodes(webpage, ".sold-out .condition-desc-con .text-con")
  nfsnode <- html_nodes(webpage, ".not-for-sale")
  
  status <- ifelse(length(soldnode) > 0 | length(soldoutnode) > 0, "Sold", 
                   ifelse(length(nfsnode) > 0, "Not For Sale", "Available")) %>% 
    ifelse(length(.) == 0, NA, .)
  
  if(brand == "Meet the Posher" & !is.na(brand)) status <- "Not For Sale"
  
  itemnode <- html_nodes(webpage, "#content span")
  item_id <- html_attr(itemnode[1], "data-post-id") %>% ifelse(length(.) == 0, NA, .)
  
  imagenode <- html_nodes(webpage, ".covershot-con img")
  image_url <- html_attr(imagenode, "src")
  date_posted <-  str_extract(image_url, "[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}") %>%
    as.Date(format = "%Y/%m/%d")
  
  nwt_boutique <- NodesToText(webpage, ".condition-tags")%>% ifelse(length(.) == 0, NA, .)
  
  nwt <- ifelse(nwt_boutique == "NWT" & !is.na(nwt_boutique), TRUE, FALSE) %>% ifelse(length(.) == 0, NA, .)
  boutique <- ifelse(nwt_boutique == "Boutique" & !is.na(nwt_boutique), TRUE, FALSE) %>% ifelse(length(.) == 0, NA, .)
  
  
  # remove original price
  price <- gsub("[[:space:]].*", "", price)
  price <- gsub(",", "", price, fixed = T)
  price <- gsub("$", "", price, fixed = T) %>% as.numeric
  
  
  results <- data.frame(user = user,
                        item_id = item_id,
                        date_scraped = now,
                        title = title,
                        brand = brand,
                        category = category,
                        subcategory = subcategory,
                        price = price,
                        status = status,
                        date_posted = date_posted,
                        last_updated = last_updated,
                        nwt = nwt,
                        boutique = boutique,
                        size = size,
                        market = market,
                        color1 = color1,
                        color2 = color2,
                        description = description,
                        item_url = item_url,
                        stringsAsFactors = F)
  
  if(nrow(results) !=1) cat(paste("Warning: item", title, "does not have 1 entry \n"))
  
  q <- rnorm(1, sd = 0.5)
  #cat(paste("sleeping", 3+q, "seconds \n"))
  Sys.sleep(3+q)
  
  return(results)
  
}


# Function to scrape each box in the search results
ScrapeSearchResults <- function(node){
  i <<- i+1
  if(floor(i/100) == i/100) cat(paste("Scraping item", i,"\n"))
  
  
  link <- html_nodes(node, "a.covershot-con") %>% 
    html_attr("href") %>% 
   as.character()
  
  title <- NodesToText(node, ".title-condition-con a") 
  date_posted_raw <- html_attr(node, "data-created-at") 
  date_posted <- date_posted_raw %>%
    str_extract( "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}") %>%
    as.Date(format = "%Y-%m-%d")
  
  user <- html_attr(node, "data-creator-handle") 
  item_id <- html_attr(node, "id") 
  brand <- html_attr(node, "data-post-brand") 
  price <- html_attr(node, "data-post-price")
  price <- gsub("$", "", price, fixed = T) %>% gsub(",", "", ., fixed = T) %>% as.numeric()
  size <- html_attr(node, "data-post-size")
  
  nwtnode <- html_nodes(node, ".title-condition-con .condition")
  nwt_text <- html_text(nwtnode)
  nwt_text <- ifelse(length(nwt_text) == 0, NA, nwt_text)
  
  nwt <- ifelse(nwt_text == "NWT" & !is.na(nwt_text), TRUE, FALSE)
  boutique <- ifelse(nwt_text == "BOUTIQUE" & !is.na(nwt_text), TRUE, FALSE)
  
  soldnode <- html_nodes(node, "a.covershot-con .sold-tag")
  soldoutnode <- html_nodes(node, "a.covershot-con .sold-out-tag")
  nfsnode <- html_nodes(node, "a.covershot-con .not-for-sale-tag")
  
  status <- ifelse(length(soldnode) > 0 | length(soldoutnode) > 0, "Sold", 
                   ifelse(length(nfsnode) > 0, "Not For Sale", "Available"))
  
  if(brand == "Meet the Posher" & !is.na(brand)) status <- "Not For Sale"
  
  results <- data.frame(user = user,
                        item_id = item_id,
                        title = title,
                        brand = brand,
                        price = price,
                        size = size,
                        status = status,
                        nwt = nwt,
                        boutique = boutique,
                        date_posted = date_posted,
                        item_url = link,
                        stringsAsFactors = F)
  if(nrow(results) !=1) cat(paste("Warning: item", title, "does not have 1 entry \n"))
  
  return(results)
}

## Wrapper Functions
ScrapeSearchResultsWrap <- function(search_url, n = "all"){
  webpage <- read_html(search_url)
  scrape_time <- Sys.time()
  box_html <- html_nodes(webpage, ".tile")
  i <<- 0
  num <- ifelse(n == "all", length(box_html), n)
  cat(paste("Scraping ", num, "items: \n"))
  scraped_results <- lapply(box_html[1:num], ScrapeSearchResults) %>% bind_rows
  
  scraped_results$scrape_time <- scrape_time
  scraped_results$search_url <- search_url
  
  q <- rnorm(1, sd = 0.5)
  #cat(paste("sleeping", 3+q, "seconds \n"))
  Sys.sleep(3+q)
  
  return(scraped_results)
  
}

# Function to scrape item - wrapper
ScrapeItemWrap <- function(links){
  i <<- 0
  scraped_results <- lapply(links, ScrapeItem) %>% bind_rows
  return(scraped_results)
}


# Function to clean the last updated field to get the date sold
DateSold <- function(item, closet){
  
  status <- closet %>% filter(item_id == item) %>% pull(status)
  if(status != "Sold"){
    date_sold <- NA
    return(date_sold)
    next
  }
  
  updated <- closet %>% filter(item_id == item) %>% pull(last_updated)
  scrape <- closet %>% filter(item_id == item) %>% pull(date_scraped)
  listing_date <- closet %>% filter(item_id == item) %>% pull(date_posted)
  
  time_words <- c("hours", "days", "Yesterday", "mins", "secs", "moments")
  time_regex <- paste(time_words, collapse = "|")
  
  updated_ago <- grepl(time_regex, updated, ignore.case = TRUE)
  
  # For listings updated days/hours ago
  if(updated_ago == TRUE){
    
    unit <- str_extract(updated, time_regex)
    amount <- str_extract(updated, "[[:digit:]]") %>% as.numeric
    
    if(unit == "hours") date_sold <- scrape - hours(amount)
    if(unit == "days")  date_sold <- scrape - days(amount)
    if(unit == "mins")  date_sold <- scrape - minutes(amount)
    if(unit == "secs")  date_sold <- scrape - seconds(amount)
    if(unit == "Yesterday") date_sold <- scrape - days(1)
    if(unit == "moments") date_sold <- scrape
    
    date_sold <- as.Date(date_sold)
    
    
  } else {
    # For listings updated at a particular date/time 
    months <-  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    month_regex <- paste(months, collapse = "|")
    
    month <- str_extract(updated, month_regex)
    month <- match(month,month.abb)
    day <- str_extract(updated, "[[:digit:]]+") %>% as.numeric
    year <- year(listing_date)
    if(month == 2 & day == 29 & year == 2015) year <- 2016
    
    date_sold <- paste(month, day, year, sep = "-")
    date_sold <- as.Date(date_sold, format = "%m-%d-%Y")
    
    if(is.na(date_sold)) cat(paste("date sold is NA, item is", item, "\n"))
    
    # Infer the year sold as the next year after the listing
    if(date_sold < listing_date){
      year <- year(listing_date)+1
      date_sold <- paste(month, day, year, sep = "-")
      date_sold <- as.Date(date_sold, format = "%m-%d-%Y")
    }
  }
  return(date_sold)
}
