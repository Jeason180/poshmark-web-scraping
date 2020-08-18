library(rvest)
library(dplyr)
library(stringr)

rm(list = ls())
setwd("C:/Users/maran/Dropbox/Web Scraping")
source("./code/Posh Scraping Functions.R")

mogi_path <- "https://poshmark.com/closet/mogibeth"
maranna_path <- "./input/individual closets/maranna_yo_20200402.html"
beckypark_path <- "./input/individual closets/beckypark_20191223.html"
emptyhanger_path <- "./input/individual closets/emptyhanger_20191215.html"
nicole_path <- "./input/individual closets/nicolestate_20191216.html"


# Scraping closet links
mogi_closet_links <- ScrapeSearchResultsWrap(mogi_path)
maranna_closet_links <- ScrapeSearchResultsWrap(maranna_path)
beckypark_closet_links <- ScrapeSearchResultsWrap(beckypark_path)
emptyhanger_closet_links <- ScrapeSearchResultsWrap(emptyhanger_path)
nicole_closet_links <- ScrapeSearchResultsWrap(nicole_path)

save(mogi_closet_links, file = "./inter/individual closets/mogi_closet_links.RDa")
save(maranna_closet_links, file = "./inter/individual closets/maranna_closet_links.RDa")
save(emptyhanger_closet_links, file = "./inter/individual closets/emptyhanger_closet_links.RDa")
save(beckypark_closet_links, file = "./inter/individual closets/beckypark_closet_links.RDa")
save(nicole_closet_links, file = "./inter/individual closets/nicole_closet_links.RDa")


# Scraping items for self
links_to_scrape <- maranna_closet_links$item_url
maranna_closet <- ScrapeItemWrap(links_to_scrape)
save(maranna_closet, file = "./inter/individual closets/maranna_closet.RDa") 



# Scraping Mogi
load("./inter/individual closets/mogi_closet_links.RDa")
links_to_scrape <- mogi_closet_links$item_url

ptm <- proc.time()
mogi_closet_part2 <- ScrapeItemWrap(links_to_scrape[1001:3291])
ptm - proc.time()

save(mogi_closet_part2, file = "./inter/individual closets/mogi_closet_20191212_1001toend.RDa") 


# Scraping empty hanger
load("./inter/individual closets/emptyhanger_closet_links.RDa")
links_to_scrape <- emptyhanger_closet_links$item_url

ptm <- Sys.time()
emptyhanger_closet_part13 <- ScrapeItemWrap(links_to_scrape[6001:7000])
emptyhanger_closet_part14 <- ScrapeItemWrap(links_to_scrape[7001:8000])
emptyhanger_closet_part15 <- ScrapeItemWrap(links_to_scrape[8001:9000])
emptyhanger_closet_part16 <- ScrapeItemWrap(links_to_scrape[9001:10000])
emptyhanger_closet_part17 <- ScrapeItemWrap(links_to_scrape[10001:11000])
emptyhanger_closet_part18 <- ScrapeItemWrap(links_to_scrape[11001:12000])
emptyhanger_closet_part19 <- ScrapeItemWrap(links_to_scrape[12001:12361])
ptm - Sys.time()

emptyhanger_closet_time3 <- bind_rows(emptyhanger_closet_part13, emptyhanger_closet_part14, emptyhanger_closet_part15,
                                   emptyhanger_closet_part16,emptyhanger_closet_part17,emptyhanger_closet_part18,
                                   emptyhanger_closet_part19)

save(emptyhanger_closet_time3, file = "./inter/individual closets/emptyhanger_closet_20191215_6001toend.RDa") 

emptyhanger_closet <- bind_rows(emptyhanger_closet_time1, emptyhanger_closet_time2, emptyhanger_closet_time3)
emptyhanger_closet <- emptyhanger_closet %>% arrange(status, desc(date_posted))
save(emptyhanger_closet, file = "./inter/individual closets/emptyhanger_closet_20191215.RDa") 



# Scraping Nicole State
load("./inter/individual closets/nicole_closet_links.RDa")
links_to_scrape <- nicole_closet_links$item_url

ptm <- Sys.time()
nicole_closet_part1 <- ScrapeItemWrap(links_to_scrape[1:1000])
nicole_closet_part2 <- ScrapeItemWrap(links_to_scrape[1001:2000])
nicole_closet_part3 <- ScrapeItemWrap(links_to_scrape[2001:3000])
nicole_closet_part4 <- ScrapeItemWrap(links_to_scrape[3001:4000])
nicole_closet_part5 <- ScrapeItemWrap(links_to_scrape[4001:5000])
nicole_closet_part6 <- ScrapeItemWrap(links_to_scrape[5001:6000])
nicole_closet_part7 <- ScrapeItemWrap(links_to_scrape[6001:6772])
ptm - Sys.time()

nicole_closet <- bind_rows(nicole_closet_part1, nicole_closet_part2, nicole_closet_part3,
                           nicole_closet_part4, nicole_closet_part5, nicole_closet_part6,
                           nicole_closet_part7)

save(nicole_closet, file = "./inter/individual closets/nicole_closet_20191216.RDa") 




# Scraping beckypark
load("./inter/individual closets/beckypark_closet_links.RDa")
links_to_scrape <- beckypark_closet_links$item_url

ptm <- Sys.time()
beckypark_closet_part1 <- ScrapeItemWrap(links_to_scrape[1:1000])
beckypark_closet_part2 <- ScrapeItemWrap(links_to_scrape[1001:2000])
beckypark_closet_part3 <- ScrapeItemWrap(links_to_scrape[2001:2604])
ptm - Sys.time()

beckypark_closet <- bind_rows(beckypark_closet_part1, beckypark_closet_part2, beckypark_closet_part3)
save(beckypark_closet, file = "./inter/individual closets/beckypark_closet_20191223.RDa") 
