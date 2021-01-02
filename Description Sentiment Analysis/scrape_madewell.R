library(DBI)
library(RPostgres)
library(tidyverse)
library(magrittr)
library(rvest)
library(lubridate)
library(tidytext)


# Preliminaries
setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()
source("./../code_git repo/Posh Scraping Functions.R")

# Connect to database
db <- "poshmark" # provide the name of your db
host_db <- "localhost" # i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
db_port <- "5432" # or any other port specified by the DBA
db_user <- "postgres"
db_password <- "Poshmark"

con <- dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)


# Get all Madewell Jeans

posh_sales <- dbGetQuery(con, "SELECT * FROM solds WHERE category = 'Jeans' AND market = 'Women' AND brand = 'Madewell'")
posh_sales %<>% filter(date_sold >= "2020-11-01")

# links
links <- posh_sales$item_url
links %<>% paste0("http://www.poshmark.com", .)

# randomly sample
set.seed(42)
links_to_scrape <- links[1:length(links)]

start_time <- Sys.time()
closet <- ScrapeItemWrap(links_to_scrape)
end_time <- Sys.time()
end_time - start_time

closet %<>% mutate(
  date_sold = map_dbl(item_id, DateSold, closet = closet),
  date_sold = as.Date(date_sold, origin = "1970-01-01")
)

save(closet, file = "./sentiment/madewell_part3.RDa")


## Read and compile scraped parts
files <- paste0("./sentiment/madewell_part", 1:3, ".RDa")

load_closet <- function(file){
  load(file)
  return(closet)
}

closet <- lapply(files, load_closet) %>% bind_rows 

save(closet, file = "./sentiment/madewell_compiled.RDa")




words_tidy <- closet %>%
  select(item_id, title, description) %>%
  unnest_tokens(word, description) %>%
  mutate(is_stop = word %in% stop_words$word)


# NRC lexicon
nrc_lexicon <- get_sentiments("nrc")

nrc_emotion <- nrc_lexicon %>% 
  filter(sentiment != "positive" & sentiment !="negative") %>% 
  group_by(word) %>% 
  summarize(emotion = "emotion")

# Join by NRC lexicon
words_analysis <- words_tidy %>%
  left_join(nrc_emotion, by = "word")

remove_words <- c("black", "blue", "white", "dark", "distressed", "distressing", "edition")

words_count_nrc <- words_analysis %>%
  filter(!(word %in% remove_words)) %>%
  group_by(item_id, title) %>%
  summarize(emotion_words = sum(!is.na(emotion)),
            all_words = n())
  

nrc_positive <- nrc_lexicon %>% 
  filter(sentiment == "positive" | sentiment =="negative") %>% 
  group_by(word) %>% 
  summarize(emotion = "emotion")



# afinn lexicon
afinn_lexicon <- get_sentiments("afinn")


