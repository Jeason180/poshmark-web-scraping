library(DBI)
library(RPostgres)
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)


# Preliminaries
setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()

# File path to blog post folder
#post_path <- "./../Blog Posts/Part 3_Overall Trends"
post_path <- "./../../Website Data/static/data/2020.11.12_poshmark_part3"

# Connect to database
db <- "poshmark" # provide the name of your db
host_db <- "localhost" # i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
db_port <- "5432" # or any other port specified by the DBA
db_user <- "postgres"
db_password <- "Poshmark"

con <- dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)






#### Sales Graph
sales <- dbGetQuery(con, "SELECT date_sold, market, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market")

sales %<>% mutate(count = as.numeric(count),
                  week = lubridate::week(date_sold)) %>%
  arrange(date_sold, market) %>%
  filter(!(date_sold >= "2020-05-10" & date_sold <= "2020-05-13" & market == "Women")) %>%
  filter(!(date_sold >= "2020-06-25" & date_sold <= "2020-07-15" & market == "Women")) %>%
  filter(!(date_sold >= "2020-10-18" & date_sold <= "2020-10-21" & market == "Women")) %>%
  group_by(market) %>% 
  mutate(roll_sum   = rollmean(sum,   7, na.pad = T, align = "right"),
         roll_count = rollmean(count, 7, na.pad = T, align = "right"))







#### Category Graph
category_sales <- dbGetQuery(con, "SELECT date_sold, market, category, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market, category")

category_sales %<>% mutate(count = as.numeric(count),
                           week = lubridate::week(date_sold)) %>%
  arrange(date_sold, market, category) %>%
  filter(!(date_sold >= "2020-05-10" & date_sold <= "2020-05-13" & market == "Women" & category %in% c("Shoes", "Dresses"))) %>%
  filter(!(date_sold >= "2020-06-25" & date_sold <= "2020-07-15" & market == "Women" & category %in% c("Shoes", "Dresses"))) %>%
  filter(!(date_sold >= "2020-10-19" & date_sold <= "2020-10-21" & market == "Women" & category %in% c("Shoes"))) %>%
  group_by(market, category) %>% 
  mutate(roll_sum   = rollmean(sum,   7, na.pad = T, align = "right"),
         roll_count = rollmean(count, 7, na.pad = T, align = "right")) %>%
  mutate(short_label = word(category, 1))


# remove some days where category had error
category_sales %<>%
  mutate(
    roll_sum =
      replace(roll_sum, (category == "Pants & Jumpsuits" &
        market == "Women" &
        date_sold >= "2020-06-04" &
        date_sold <= "2020-07-28"), NA))





### Subcategory data
subcategory_sales <- dbGetQuery(con, "SELECT date_sold, market, category, subcategory, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market, category, subcategory")

subcategory_sales %<>% mutate(count = as.numeric(count),
                              week = lubridate::week(date_sold)) %>%
  filter(market == "Women" & category %in% c("Dresses", "Tops", "Shoes", "Pants & Jumpsuits", "Jeans")) %>%
  arrange(date_sold, market, category, subcategory) %>%
  filter(!(date_sold >= "2020-06-25" & date_sold <= "2020-07-15" & market == "Women" & category %in% c("Shoes", "Dresses"))) %>%
  filter(!(date_sold >= "2020-10-18" & date_sold <= "2020-10-21" & market == "Women" & category %in% c("Shoes"))) %>%
  group_by(market, category, subcategory) %>% 
  mutate(roll_sum   = rollmean(sum,   7, na.pad = T, align = "right"),
         roll_count = rollmean(count, 7, na.pad = T, align = "right"))



# Days to sell
days_to_sell <- dbGetQuery(con, "SELECT market, days_to_sell, COUNT(item_id), SUM(price) FROM solds GROUP BY market, days_to_sell")

days_to_sell_avg <- dbGetQuery(con, "SELECT market, AVG(days_to_sell) FROM solds WHERE days_to_sell <= 365 GROUP BY market")



# Sizes
sizes <- dbGetQuery(con, "SELECT market, category, size, COUNT(item_id), SUM(price), SUM(days_to_sell) AS sum_dts FROM solds WHERE days_to_sell < 365 GROUP BY market, category, size")

sizes %<>% filter(count > 1) %>% arrange(-count)

# Standard sizes
sizes_standard_vec <- c("XXS", "XS", "S", "M", "L", "XL")
sizes_dress_vec <- c("00", as.character(seq(from = 0, to = 12, by = 2)))
sizes_jeans_vec <- 23:31
sizes_standard_all_vec <- c(sizes_standard_vec, sizes_dress_vec, sizes_jeans_vec)

sizes_ambiguous <- c(24, 26, 28, 30)
sizes_standard_all_diff <- setdiff(sizes_standard_all_vec, sizes_ambiguous) # ambiguous sizes

# Plus sizes
sizes_plusnum_vec <- seq(from = 14, to = 32, by = 2)
sizes_plusw_vec <- paste0(sizes_plusnum_vec, "W")
sizes_pluschar_vec <- c("XXL", "XXXL", paste0(0:5, "X"))
sizes_plus_vec <- c(sizes_plusnum_vec, sizes_plusw_vec, sizes_pluschar_vec, 32:34)

# Petite sizes
sizes_petite_vec <- c("00", seq(from = 0, to = 20, by = 2), "XXS", "XS", "S", "M", "L", "XL", "XXL", 23:34)
sizes_petite_vec %<>% paste0(., "P")

# Shoe Sizes
sizes_shoes_vec <- seq(from = 5, to = 13, by = 0.5)


# Tag each size by whether it is plus or petite
sizes_plus_petite <- sizes %>%
  filter(market == "Women") %>%
  filter(!(category %in% c("Shoes"))) %>%
  mutate(size = as.character(size)) %>%
  mutate(size_type = case_when(size %in% sizes_standard_all_diff ~ "Standard",
                               size %in% sizes_plus_vec ~ "Plus",
                               size %in% sizes_petite_vec ~ "Petite")) %>%
  mutate(size_type = if_else((size %in% sizes_ambiguous & category %in% c("Jeans", "Pants & Jumpsuits", "Shorts")),
                             NA_character_, size_type))  # remove ambiguous sizes

# Function to summarize size info
CollapseSizes <- function(data){
  results <- data %>%
    group_by(market, size) %>%
    summarize(total_items = sum(count),
              total_sales = sum(sum),
              total_dts   = sum(sum_dts)) %>%
    mutate(asp = total_sales / total_items,
           avg_days = total_dts / total_items)
  
  return(results)
}

# Size detail
sizes_pp_detail <- sizes_plus_petite %>%
  filter(!is.na(size_type)) %>%
  group_by(market, size, size_type) %>%
  summarize(total_items = sum(count),
            total_sales = sum(sum),
            total_dts   = sum(sum_dts)) %>%
  mutate(asp = total_sales / total_items,
         avg_days = total_dts / total_items) %>%
  filter(total_items >= 500)

# sizes for shoes
sizes_shoes <- sizes %>%
  filter(market == "Women") %>%
  filter(size %in% sizes_shoes_vec) %>%
  filter(category == "Shoes") %>%
  CollapseSizes()



# Brands
brand_sales <- dbGetQuery(con, "SELECT brand, market, category, SUM(price), count(item_id) FROM solds GROUP BY brand, market, category")

brand_sales %<>% filter(count >=5) %>% arrange(desc(sum))

brand_sales_overall <- dbGetQuery(con, "SELECT brand, SUM(price), count(item_id) FROM solds GROUP BY brand")
brand_sales_overall %<>% filter(count >=5) %>% arrange(desc(sum))






save(sales, category_sales, subcategory_sales, days_to_sell, brand_sales, sizes_pp_detail, sizes_shoes, file = paste0(post_path, "/objects_for_post.RDa"))





