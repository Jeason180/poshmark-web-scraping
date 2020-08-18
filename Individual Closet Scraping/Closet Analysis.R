library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(RColorBrewer)
library(gt)

rm(list = ls())
setwd("C:/Users/maran/Dropbox/Web Scraping")


# Load closets
paths <- c("./output/closets/mogi_closet_20200721.RDa",
           "./output/closets/nicole_closet_20200723.RDa",
           "./output/closets/emptyhanger_closet_20200724.RDa")

LoadFunction <- function(path){
  load(path)
  return(combined_data)
}
closet_data <- lapply(paths, LoadFunction) %>% bind_rows

# Impute Poshmark fees:
closet_data %<>% mutate(
  revenue = ifelse(price >= 15, price * 0.8, price - 2.95))



### Get currently active listings
active_summary <- closet_data %>%
  filter(status == "Available") %>%
  group_by(user) %>%
  summarize(active_listings = n())


#####
#####
#####  Solds Analysis
#####
#####

# First, let's filter only sold items and filter only on timeframe of interest
date_start <- "2019-07-01"
date_end <- "2020-06-30"

solds <- closet_data %>% 
  filter(status == "Sold") %>%
  filter(date_sold >= date_start & date_sold <= date_end)



#####
#####  Section 1: Annual Stats
#####


# Calculate total sales volume and items sold for each user, and calculate average sale price:
solds_yearly <- solds %>%
  group_by(user) %>%
  summarize(
    total_sales   = sum(price)/1000,
    total_revenue = sum(revenue)/1000,
    items_sold    = n()) %>%
  mutate(asp      = total_sales*1000 / items_sold)

palette <- brewer.pal(8,"Set2")

# Let's graph this
sales_graph <- ggplot(data = solds_yearly, aes(x = user, y = total_sales)) +
  geom_col(fill = palette[1]) + 
  ggtitle("July 2019 - June 2020 Sales") +
  ylab("Annual sales, thousands") + 
  coord_flip() 

volume_graph <- ggplot(data = solds_yearly, aes(x = user, y = items_sold)) +
  geom_col(fill = palette[2]) + 
  ggtitle("July 2019 - June 2020 Items Sold") +
  ylab("Items Sold") + 
  coord_flip() 

asp_graph <- ggplot(data = solds_yearly, aes(x = user, y = asp)) +
  geom_col(fill = palette[3]) + 
  ggtitle("July 2019 - June 2020 Average Sales Price") +
  ylab("Price ($)") + 
  coord_flip() 

sales_graph
volume_graph
asp_graph


#####
#####  Section 2: Category Analysis
#####

# Get the revenue sold by each category and rank them
category_analysis <- solds %>%
  group_by(user, market, category) %>%
  summarize(
    total_sales   = sum(price)/1000,
    total_revenue = sum(revenue)/1000,
    items_sold    = n()) %>%
  mutate(asp      = total_sales*1000 / items_sold) %>%
  arrange(user, desc(total_sales)) %>%
  ungroup() %>%
  group_by(user) %>%
  mutate(rank = rank(-total_sales))


# Filter down to top 5 categories:
category_top5 <- category_analysis %>% 
  group_by(user) %>%
  mutate(percent = total_sales / sum(total_sales)) %>%
  filter(rank <= 5)
 

# Create formatted table using gt:
category_table  <- category_top5 %>%
  select(-total_revenue) %>%
  select(rank, everything()) %>%
  gt(rowname_col = "rank", groupname_col = "user") %>%
  fmt_number(columns = vars(asp), decimals = 2) %>%
  fmt_number(columns = vars(total_sales), scale_by = 1000, decimals = 0) %>%
  fmt_number(columns = vars(items_sold), decimals = 0) %>%
  fmt_percent(columns = vars(percent), decimals = 0) %>%
  tab_header("Top 5 Categories For Each Reseller") %>%
  cols_label(rank = "Rank",
             market = "Market",
             category = "Category",
             total_sales = "Annual Sales",
             items_sold = "Items Sold",
             asp = "Average Price",
             percent = "Percent of Sales")

#summary_rows(groups = TRUE, columns = vars(total_sales, items_sold, percent), fns = list(Total = "sum"))

category_table 


#####
#####  Section 3: Brand Analysis
#####

# Get the revenue sold by each category and rank them
brand_analysis <- solds %>%
  group_by(user, brand) %>%
  summarize(
    total_sales   = sum(price)/1000,
    total_revenue = sum(revenue)/1000,
    items_sold    = n()) %>%
  mutate(asp      = total_sales*1000 / items_sold) %>%
  arrange(user, desc(total_sales)) %>%
  ungroup() %>%
  group_by(user) %>%
  mutate(rank = rank(-total_sales))


# Filter down to top 10 brands:
brand_top10 <- brand_analysis %>% 
  group_by(user) %>%
  mutate(percent = total_sales / sum(total_sales)) %>%
  filter(rank <= 10)


# Create formatted table using gt:
brand_table  <- brand_top10 %>%
  select(-total_revenue, -items_sold, -asp) %>%
  select(rank, everything()) %>%
  gt(rowname_col = "rank", groupname_col = "user") %>%
  fmt_number(columns = vars(total_sales), scale_by = 1000, decimals = 0) %>%
  fmt_percent(columns = vars(percent), decimals = 0) %>%
  tab_header("Top 10 Brands For Each Reseller") %>%
  cols_label(rank = "Rank",
             brand = "Brand",
             total_sales = "Annual Sales",
             percent = "Percent of Sales")

brand_table 
