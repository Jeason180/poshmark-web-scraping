library(DBI)
library(RPostgres)
library(tidyverse)
library(magrittr)
library(lubridate)
library(directlabels)


# Preliminaries
setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()



# Connect to database
db <- "poshmark" # provide the name of your db
host_db <- "localhost" # i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
db_port <- "5432" # or any other port specified by the DBA
db_user <- "postgres"
db_password <- "Poshmark"

con <- dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)



# total sales over time
sales <- dbGetQuery(con, "SELECT date_sold, market, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market")

sales %<>% mutate(count = as.numeric(count),
                  week = lubridate::week(date_sold)) %>%
  arrange(date_sold, market) %>%
  group_by(market) %>% 
  mutate(roll_sum = (sum + lag(sum) + lag(sum, 2))/3,
         roll_count = (count + lag(count)+ lag(count,2))/3)


sales_graph <- sales %>% filter(date_sold >= "2020-05-01") %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = market), size = 1.3) +
  labs(title = "Total Poshmark Clothing Sales (3 Day Rolling Average)", 
       x = "Date", y = "Average Daily Sales ($ thousands)")


sales_graph


# by category
category_sales <- dbGetQuery(con, "SELECT date_sold, market, category, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market, category")

category_sales %<>% mutate(count = as.numeric(count),
                           week = lubridate::week(date_sold)) %>%
  arrange(date_sold, market, category) %>%
  group_by(market, category) %>% 
  mutate(roll_sum = (sum + lag(sum) + lag(sum, 2))/3,
         roll_count = (count + lag(count)+ lag(count,2))/3) %>%
  mutate(short_label = word(category, 1))


# remove some days where pants category had error
category_sales %<>%  mutate(roll_sum = replace(roll_sum, (category == "Pants & Jumpsuits" & 
                              market == "Women" & 
                              date_sold >= "2020-06-04" &
                              date_sold <= "2020-07-28"), NA))

category_graph_w <- category_sales %>% filter(date_sold >= "2020-05-01" & market == "Women") %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = category), size = 1.3) +
  scale_x_date(expand = expansion(add = c(0,15)))  + 
  geom_dl(aes(label = short_label), method = list(dl.trans(x = x + 0.1), "last.qp", cex = 0.8)) +
  labs(title = "Women's Clothing Poshmark Sales (3 Day Rolling Average)", x = "Date", y = "Average Daily Sales ($ thousands)")

category_graph_w


category_graph_m <- category_sales %>% filter(date_sold >= "2020-05-01" & market == "Men") %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = category), size = 1.3) +
  scale_x_date(expand = expansion(add = c(0,15)))  + 
  geom_dl(aes(label = short_label), method = list(dl.trans(x = x + 0.1), "last.qp", cex = 0.8)) +
  labs(title = "Men's Clothing Poshmark Sales (3 Day Rolling Average)", x = "Date", y = "Average Daily Sales ($ thousands)")

category_graph_m


# Subcategory
subcategory_sales <- dbGetQuery(con, "SELECT date_sold, market, category, subcategory, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market, category, subcategory")

subcategory_sales %<>% mutate(count = as.numeric(count),
                           week = lubridate::week(date_sold)) %>%
  filter(!is.na(subcategory)) %>%
  arrange(date_sold, market, category, subcategory) %>%
  group_by(market, category, subcategory) %>% 
  mutate(roll_sum = (sum + lag(sum) + lag(sum, 2))/3,
         roll_count = (count + lag(count)+ lag(count,2))/3) 

# Graph
subcategory_graph_w <- subcategory_sales %>% filter(date_sold >= "2020-07-20" & category == "Tops") %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = subcategory), size = 1.3) +
  scale_x_date(expand = expansion(add = c(0,15)))  + 
  geom_dl(aes(label = subcategory), method = list(dl.trans(x = x + 0.1), "last.qp", cex = 0.8)) +
  labs(title = "Women's Clothing Poshmark Sales (3 Day Rolling Average)", x = "Date", y = "Average Daily Sales ($ thousands)")

subcategory_graph_w


# Top brands
brand_sales <- dbGetQuery(con, "SELECT brand, market, SUM(price), count(item_id) FROM solds GROUP BY brand, market")

brand_sales %<>% filter(count >=5) %>% arrange(desc(sum))


# lululemon vs all other athletic pants 
pants_sales <- dbGetQuery(con, "SELECT date_sold, market, category, SUM(price), count(item_id) FROM solds WHERE category = 'Pants & Jumpsuits' GROUP BY date_sold, market, category")
lulu_sales  <- dbGetQuery(con, "SELECT date_sold, market, category, brand, SUM(price), count(item_id) FROM solds WHERE category = 'Pants & Jumpsuits' AND brand = 'lululemon athletica' GROUP BY date_sold, market, category, brand")

pants_sales %<>% rename(sum_all = sum, count_all = count)
lulu_sales %<>% select(date_sold, sum, count) %>% rename(sum_lulu = sum, count_lulu = count)
pants_sales %<>% left_join(lulu_sales, by = "date_sold")






