# Poshmark-Web-Scraping

This repository has the code for my Poshmark web scraping project. For a write up of the results, see my [website](http://marannadata.rbind.io/).

The code file "Posh Scraping Functions.R" is the key code file with my functions for scraping Poshmark closet/search results pages, individual item pages, and helper and wrapper functions. These functions are used to scrape individual sellers and a selection of recently sold items.

The folder "Individual closet scraping" has code relevant to the scraping of individual sellers' items. "Posh Scraper Initial.R" and "Posh Scraper Update.R" contain code for my initial scrape from sellers' listings, and periodic updates, respectively, utilizing functions from "Posh Scraping Functions.R" These code files are relevant for Parts 1 and 2 of my Poshmark blog post series.

The folder "Just sold items scraper" has code relevant to scraping of recently sold items. The key code file is "Just_Sold_Scraper_Base_Code.R" which applies my scraping functions to the input list of URLs to scrape. The 4 code files beginning with "Wrap_" just call "Just_Sold_Scraper_Base_Code.R" and correspond to 4 scheduled tasks with different URL inputs and scheduled at different frequencies. The code file "Deduper Function.R" compiles each month's individual scraped files into a monthly file. The file "File Issues Summary.R" counts the number of scraped files that exist and compares them to the number that should exist, which helps me quickly diagnose which days and months contain missing data. These code files are relevant for Part 3 of my Poshmark blog post series.

The folder "Exploratory" contains code relevant to exploring the data. The code file "exploratory graphs.R" contains graphs used for understanding broader trends in Poshmark data, as I write about in Part 3 of my Poshmark blog post series.

The folder "SQL" contains code used to import my Poshmark data into PostgreSQL 13.

The folder "Modeling" contains code under development to train and fit machine learning models to the Poshmark data.

The folder "Price rescrape" contains code relating to rescraping missing price, size, and subcategory information due to a past coding errors and compiling them with the correctly scraped data.

