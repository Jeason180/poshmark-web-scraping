# Tutorial from 
# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/

library(rvest)
library(dplyr)

rm(list = ls())

url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

webpage <- read_html(url)
rank_data_html <- html_nodes(webpage, ".text-primary")
rank_data <-  rank_data_html %>% html_text %>% as.numeric

title_data_html <- html_nodes(webpage, ".lister-item-header a")
title_data <- title_data_html %>% html_text

# Speed it up into a function
NodesToText <- function(page, tag){
  html_data <- html_nodes(page, tag)
  text <- html_text(html_data)
  return(text)
}

description_data <- NodesToText(webpage, ".ratings-bar+ .text-muted")
runtime_data <- NodesToText(webpage, ".text-muted .runtime")
genre_data <- NodesToText(webpage, ".genre")
rating_data <- NodesToText(webpage, ".ratings-imdb-rating strong")
votes_data <- NodesToText(webpage, ".sort-num_votes-visible span:nth-child(2)")
metascore_data <- NodesToText(webpage, ".metascore")
gross_data <- NodesToText(webpage, ".ghost~ .text-muted+ span")

