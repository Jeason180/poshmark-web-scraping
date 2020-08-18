# Scraping my own poshmark closet


library(rvest)
library(dplyr)
library(stringr)

rm(list = ls())


# Function to scrape a URL
url <- "https://poshmark.com/listing/Gap-Striped-Summer-Dress-5dd15cc07a8173415da0485f"

webpage <- read_html(url)
now <- Sys.time()

# Speed it up into a function
NodesToText <- function(page, tag){
  html_data <- html_nodes(page, tag)
  text <- html_text(html_data)
  return(text)
}


title <- NodesToText(webpage, ".details .title")
brand <- NodesToText(webpage, ".details .brand")
price <- NodesToText(webpage, ".details .price")
size <- NodesToText(webpage, ".details .size-box")
description <- NodesToText(webpage, ".description")
market <- NodesToText(webpage, ".tag-list a[data-pa-name*='category']:first-child")
category <- NodesToText(webpage, ".tag-list a[data-pa-name*='category']:nth-child(2)")
subcategory <- NodesToText(webpage, ".tag-list a:nth-child(3)")
color1 <- NodesToText(webpage, ".tag-list a[data-pa-name*='color']:first-child")
color2 <- NodesToText(webpage, ".tag-list a[data-pa-name*='color']:nth-child(2)")
user <- NodesToText(webpage, ".creator-details .handle")[1]
last_updated <- NodesToText(webpage, ".creator-details .time")[1]

soldnode <- html_nodes(webpage, ".sold .condition-desc-con .text-con")
sold <- ifelse(length(soldnode) > 0, "Sold", "Available")

itemnode <- html_nodes(webpage, "#content span")
item_id <- html_attr(itemnode[1], "data-post-id")

imagenode <- html_nodes(webpage, ".covershot-con img")
image_url <- html_attr(imagenode, "src")
date_posted <-  str_extract(image_url, "[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}") %>%
  as.Date(format = "%Y/%m/%d")

# remove original price
price <- gsub("[[:space:]].*", "", price)
price <- gsub("$", "", price, fixed = T) %>% as.numeric

results <- data.frame(user = user,
                      item_id = item_id,
                      date_scraped = now,
                      title = title,
                      brand = brand,
                      category = category,
                      subcategory = subcategory,
                      price = price,
                      sold = sold,
                      date_posted = date_posted,
                      last_updated = last_updated,
                      size = size,
                      market = market,
                      color1 = color1,
                      color2 = color2,
                      description = description)


# Scrape the search results

url <- "./input/individual closets/mogibeth_20191212.html"

webpage <- read_html(url)
box_html <- html_nodes(webpage, ".tile")
node <- box_html[704]

now <- Sys.time()

link <- html_nodes(node, "a.covershot-con") %>% 
  html_attr("href") %>% 
  paste0("https://poshmark.com", .)

title <- NodesToText(node, ".title-condition-con a")
date_posted_raw <- html_attr(node, "data-created-at") 
date_posted <- date_posted_raw %>%
  str_extract( "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}") %>%
  as.Date(format = "%Y-%m-%d")

user <- html_attr(node, "data-creator-handle") 
item_id <- html_attr(node, "id") 
brand <- html_attr(node, "data-post-brand") 

soldnode <- html_nodes(node, "a.covershot-con .sold-tag")
soldoutnode <- html_nodes(node, "a.covershot-con .sold-out-tag")
nfsnode <- html_nodes(node, "a.covershot-con .not-for-sale-tag")

status <- ifelse(length(soldnode) > 0 | length(soldoutnode) > 0, "Sold", 
                 ifelse(length(nfsnode) > 0, "Not For Sale", "Available"))

if(brand == "Meet the Posher") status <- "Not For Sale"

results <- data.frame(user = user,
                      item_id = item_id,
                      date_scraped = now,
                      title = title,
                      brand = brand,
                      status = status,
                      date_posted = date_posted,
                      url = link)













# For reading from the search results
savedpage <- "C:/Users/maran/Documents/Data Stuff/Web Scraping/@mogibeth) _ Poshmark_20191212.html"
webpage <- read_html(savedpage)

box_html <- html_nodes(webpage, ".tile")
node <- box_html[1]

node11 <- html_nodes(node, "")
html_text(node11)
