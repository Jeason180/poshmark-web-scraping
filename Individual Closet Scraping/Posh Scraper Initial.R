library(rvest)
library(tidyverse)
library(lubridate)
library(magrittr)

rm(list = ls())
setwd("C:/Users/maran/Dropbox/Web Scraping")
source("./code/Posh Scraping Functions.R")

# Update the handle of each closet you want to scrape.
# All other file paths update automatically, given folder structure
user <- "maranna_yo"
date <- "20200904"

handle <- paste(user, date, sep = "_")




# Path to saved HTML file of each closet
closet_path <- paste0("./input/individual closets/", user, "/closet_", handle, ".html")


# Scraping closet links
closet_links <- ScrapeSearchResultsWrap(closet_path)
save(closet_links, file = paste0("./inter/individual closets/", user, "/closet_links_", handle, ".RDa"))



# Scraping individual items from each closet
load(paste0("./inter/individual closets/", user, "/closet_links_", handle, ".RDa"))
links_to_scrape <- closet_links$item_url

start_time <- Sys.time()
closet <- ScrapeItemWrap(links_to_scrape)
end_time <- Sys.time()
end_time - start_time

closet %<>% mutate(
  date_sold = map_dbl(item_id, DateSold, closet = closet),
  date_sold = as.Date(date_sold, origin = "1970-01-01")
)

save(closet, file = paste0("./inter/individual closets/", user, "/closet_items_", handle, ".RDa"))


closet    %<>% mutate(days_to_sell = as.numeric(date_sold - date_posted),
                      month_sold = floor_date(date_sold, "month"),
                      year_sold = year(date_sold),
                      month_posted = floor_date(date_posted, "month"))

save(closet, file = paste0("./output/closets/closet_", handle, ".RDa"))
