# Preliminaries
library(readxl)

rm(list = ls())
gc()


setwd("C:/Users/maran/Dropbox/Web Scraping")
#setwd("C:/Users/Administrator/Dropbox/Web_Scraping")


# Read in list of input URLs
url_inputs <- read_excel("./input/Just Sold URLs.xlsx", sheet = "All")

source("./code_git/Just_Sold_Scraper_Base Code.R")


saveRDS(scraped_results, file = paste0("./inter/just_sold/results_", saveid,".RDS"))



                              