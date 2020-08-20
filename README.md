# Poshmark-Web-Scraping

This repository has the code for my Poshmark web scraping project.

The code file "Posh Scraping Functions.R" is the key code file with my functions for scraping Poshmark closet/search results pages, individual item pages, and helper and wrapper functions. These functions are used to scrape individual sellers and a selection of recently sold items.

The folder "Individual closet scraping" has code relevant to the scraping of individual sellers' items. "Posh Scraper Initial.R" and "Posh Scraper Update.R" contain code for my initial scrape from sellers' listings, and periodic updates, respectively, utilizing functions from "Posh Scraping Functions.R" The file "Closet Analysis.R" has some analysis code, still under development in conjunction with RMarkdown posts.

The folder "Just sold items scraper" has code relevant to scraping of recently sold items. The key code file is "Just_Sold_Scraper_Base_Code.R" which applies my scraping functions to the input list of URLs to scrape. The 4 code files beginning with "Wrap_" just call "Just_Sold_Scraper_Base_Code.R" and correspond to 4 scheduled tasks with different URL inputs and scheduled at different frequencies. The code file "Deduper Function.R" compiles each month's individual scraped files into a monthly master file.

The folder "Price rescrape" contains code relating to rescraping missing price and size information due to a past coding error.

