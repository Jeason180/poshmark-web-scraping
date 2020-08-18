# Task Scheduler Log File

setwd("C:/Users/Administrator/Dropbox/Web_Scraping")


sink("./inter/just_sold/logs/just_sold_log.txt")

source("./code/Just_Sold_Scraper_HalfHourly.R", echo = T, max.deparse.length=10000)

sink()

file.rename("./inter/just_sold/logs/just_sold_log.txt", paste0("./inter/just_sold/logs/just_sold_log_", saveid, ".txt"))
