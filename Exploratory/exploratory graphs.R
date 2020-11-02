library(DBI)
library(RPostgres)
library(tidyverse)
library(magrittr)
library(lubridate)
library(directlabels)
library(zoo)


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
  mutate(roll_sum   = rollmean(sum,   7, na.pad = T, align = "right"),
         roll_count = rollmean(count, 7, na.pad = T, align = "right"))
  


sales_graph <- sales %>% filter(date_sold >= "2020-05-01") %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = market), size = 1.3) +
  labs(title = "Total Poshmark Clothing Sales (7 Day Rolling Average)", 
       x = "Date", y = "Average Daily Sales ($ thousands)")


sales_graph









# by category
category_sales <- dbGetQuery(con, "SELECT date_sold, market, category, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market, category")

category_sales %<>% mutate(count = as.numeric(count),
                           week = lubridate::week(date_sold)) %>%
  arrange(date_sold, market, category) %>%
  group_by(market, category) %>% 
  mutate(roll_sum   = rollmean(sum,   7, na.pad = T, align = "right"),
         roll_count = rollmean(count, 7, na.pad = T, align = "right")) %>%
  mutate(short_label = word(category, 1))


# remove some days where category had error
category_sales %<>%  mutate(roll_sum = replace(roll_sum, (category == "Pants & Jumpsuits" & 
                              market == "Women" & 
                              date_sold >= "2020-06-04" &
                              date_sold <= "2020-07-28"), NA)) %>%
                      filter(!(category == "Shoes" & 
                               market == "Women" & 
                               date_sold >= "2020-06-29" &
                               date_sold <= "2020-07-18"))

category_graph_w <- category_sales %>% filter(date_sold >= "2020-05-15" & market == "Women") %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = category), size = 1.3) +
  scale_x_date(expand = expansion(add = c(0,15)))  + 
  geom_dl(aes(label = short_label), method = list(dl.trans(x = x + 0.1), "last.qp", cex = 0.8)) +
  labs(title = "Women's Clothing Poshmark Sales (7 Day Rolling Average)", x = "Date", y = "Average Daily Sales ($ thousands)")

category_graph_w


category_graph_m <- category_sales %>% filter(date_sold >= "2020-05-01" & market == "Men") %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = category), size = 1.3) +
  scale_x_date(expand = expansion(add = c(0,15)))  + 
  geom_dl(aes(label = short_label), method = list(dl.trans(x = x + 0.1), "last.qp", cex = 0.8)) +
  labs(title = "Men's Clothing Poshmark Sales (7 Day Rolling Average)", x = "Date", y = "Average Daily Sales ($ thousands)")

category_graph_m











# Subcategory
subcategory_sales <- dbGetQuery(con, "SELECT date_sold, market, category, subcategory, SUM(price), count(item_id) FROM solds GROUP BY date_sold, market, category, subcategory")

subcategory_sales %<>% mutate(count = as.numeric(count),
                           week = lubridate::week(date_sold)) %>%
  filter(market == "Women" & category %in% c("Dresses", "Tops", "Shoes", "Pants & Jumpsuits", "Jeans")) %>%
  arrange(date_sold, market, category, subcategory) %>%
  group_by(market, category, subcategory) %>% 
  mutate(roll_sum   = rollmean(sum,   7, na.pad = T, align = "right"),
         roll_count = rollmean(count, 7, na.pad = T, align = "right"))

# Graph
subcategory_graph_w <- subcategory_sales %>% filter(date_sold >= "2020-08-01" & category == "Dresses") %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = subcategory), size = 1.3) +
  scale_x_date(expand = expansion(add = c(0,15)))  + 
  geom_dl(aes(label = subcategory), method = list(dl.trans(x = x + 0.1), "last.qp", cex = 0.8)) +
  labs(title = "Women's Clothing Poshmark Sales (7 Day Rolling Average)", x = "Date", y = "Average Daily Sales ($ thousands)")

subcategory_graph_w


# Top brands
brand_sales <- dbGetQuery(con, "SELECT brand, market, SUM(price), count(item_id) FROM solds GROUP BY brand, market")

brand_sales %<>% filter(count >=5) %>% arrange(desc(sum))




# Seasonality

season_categories <- c("Jackets & Coats", "Jeans", "Sweaters", "Shorts", "Swim")

sea_category_graph <- category_sales %>% 
  filter(date_sold >= "2020-07-01" & market == "Women") %>%
  filter(category %in% season_categories) %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = category), size = 1.3) +
  scale_x_date(expand = expansion(add = c(0,15)))  + 
  geom_dl(aes(label = short_label), method = list(dl.trans(x = x + 0.1), "last.qp", cex = 0.8)) +
  labs(title = "Women's Seasonal Categories (7 Day Rolling Average)", x = "Date", y = "Average Daily Sales ($ thousands)")

sea_category_graph


season_subcat_shoes <- subcategory_sales %>% 
  filter(category == "Shoes" & market == "Women") %>%
  pull(subcategory) %>%
  grep("Boot", ., ignore.case = T, value = T) %>% 
  unique %>%
  c("Sandals")

sea_subcategory_graph_shoes <- subcategory_sales %>% 
  filter(date_sold >= "2020-07-20" & category == "Shoes" & subcategory %in% season_subcat_shoes) %>%
  ggplot(data = ., aes(x = date_sold, y = roll_sum/1000)) + 
  geom_line(aes(color = subcategory), size = 1.3) +
  labs(title = "Women's Clothing Poshmark Sales (7 Day Rolling Average)", 
       x = "Date", 
       y = "Average Daily Sales ($ thousands)")


sea_subcategory_graph_shoes



# Results by time of day, day of week

sales_time <- dbGetQuery(con, "SELECT scrape_id, sum(price) FROM solds GROUP BY scrape_id")

sales_time %<>% separate(scrape_id, into = c("date_sold", "time"), sep = "_") %>%
  mutate(date_sold = as.Date(date_sold),
         day_of_week = wday(date_sold, label = T, abbr = F)) %>%
  filter(date_sold >= "2020-05-01")

day_summary <- sales_time %>%
  group_by(date_sold, day_of_week) %>%
  summarize(sales = sum(sum)) %>%
  group_by(day_of_week) %>%
  summarize(avg_sales = median(sales),
            obs = n())

# collapse times to standardize
# group into 2 hour periods across day
time_summary <- sales_time %>%
  mutate(time_num = as.numeric(time),
         hour = floor(time_num/100),
         hour_std = if_else((hour %% 2) == 0, hour, hour-1)) %>%
  group_by(date_sold, hour_std) %>%
  summarize(sales = sum(sum)) %>%
  group_by(hour_std) %>%
  summarize(avg_sales = median(sales)) %>%
  mutate(hour_std = hour_std - 2) %>% # change to 2 hour period beginning...
  mutate(hour_std = if_else(hour_std == -2, 22, hour_std))



ggplot(day_summary, aes(x = day_of_week, y = avg_sales)) + geom_col(fill = "#00BFC4")

ggplot(time_summary, aes(x = hour_std, y = avg_sales)) + geom_line(col = "#00BFC4", size = 1.5) +
  labs(title = "Average Sales per Hour",
       x = "Hour",
       y = "Average Sales")





## Distribution of days to sell
days_to_sell <- dbGetQuery(con, "SELECT market, days_to_sell, COUNT(item_id), SUM(price) FROM solds GROUP BY market, days_to_sell")

days_to_sell %<>%
  group_by(market) %>%
  mutate(count = as.numeric(count),
         total_sold = sum(count),
         percent = count / total_sold) 

days_to_sell %>%
  filter(days_to_sell < 100) %>%
  ggplot(aes(x = days_to_sell, y = percent, col = market)) + geom_line(size = 1)

days_to_sell %<>%
  mutate(bin = cut(days_to_sell, breaks = c(0,1,5,30, 365, Inf), 
                   labels = c("0 Days", "1-5 Days", "6-30 Days", "31 Days-1 Year", "1 Year+"), 
                   include.lowest = T, right = F)) %>%
  group_by(market, bin) %>%
  summarize(cumulative_percent = sum(percent))





# Distribution of sizes
sizes <- dbGetQuery(con, "SELECT market, category, size, COUNT(item_id), SUM(price), SUM(days_to_sell) AS sum_dts FROM solds WHERE days_to_sell < 365 GROUP BY market, category, size")

sizes %<>% filter(count > 1) %>% arrange(-count)

sizes_regular_vec <- c("XXS", "XS", "S", "M", "L", "XL")
sizes_shoes_vec <- seq(from = 5, to = 13, by = 0.5)
sizes_dress_vec <- c("00", as.character(seq(from = 0, to = 12, by = 2)))
sizes_jeans_vec <- 23:31
sizes_regular_all_vec <- c(sizes_regular_vec, sizes_dress_vec, sizes_jeans_vec)

sizes_ambiguous <- c(24, 26, 28, 30)
sizes_regular_all_diff <- setdiff(sizes_regular_all_vec, sizes_ambiguous) # ambiguous sizes


sizes_plusnum_vec <- seq(from = 14, to = 32, by = 2)
sizes_plusw_vec <- paste0(sizes_plusnum_vec, "W")
sizes_pluschar_vec <- c("XXL", "XXXL", paste0(0:5, "X"))
sizes_plus_vec <- c(sizes_plusnum_vec, sizes_plusw_vec, sizes_pluschar_vec, 32:34)

sizes_petite_vec <- c("00", seq(from = 0, to = 20, by = 2), "XXS", "XS", "S", "M", "L", "XL", "XXL", 23:34)
sizes_petite_vec %<>% paste0(., "P")
  
sizes_plus_petite <- sizes %>%
  filter(market == "Women") %>%
  filter(!(category %in% c("Shoes"))) %>%
  mutate(size = as.character(size)) %>%
  mutate(size_type = case_when(size %in% sizes_regular_all_diff ~ "Regular",
                               size %in% sizes_plus_vec ~ "Plus",
                               size %in% sizes_petite_vec ~ "Petite")) %>%
  mutate(size_type = if_else((size %in% sizes_ambiguous & category %in% c("Jeans", "Pants & Jumpsuits", "Shorts")),
                             NA_character_, size_type))  # remove ambiguous sizes


sizes_pp_summary <- sizes_plus_petite %>%
  select(-size) %>%
  rename(size = size_type) %>%
  filter(!is.na(size)) %>%
  CollapseSizes()

sizes_pp_detail <- sizes_plus_petite %>%
  filter(!is.na(size_type)) %>%
  group_by(market, size, size_type) %>%
  summarize(total_items = sum(count),
            total_sales = sum(sum),
            total_dts   = sum(sum_dts)) %>%
  mutate(asp = total_sales / total_items,
         avg_days = total_dts / total_items)


sizes_regular <- sizes %>%
  filter(market == "Women") %>%
  filter(size %in% sizes_regular_vec) %>%
  filter(category != "Shoes") %>%
  CollapseSizes()


sizes_shoes <- sizes %>%
  filter(market == "Women") %>%
  filter(size %in% sizes_shoes_vec) %>%
  filter(category == "Shoes") %>%
  CollapseSizes()
  

sizes_dress <- sizes %>%
  filter(market == "Women") %>%
  filter(size %in% sizes_dress_vec) %>%
  filter(category != "Shoes") %>%
  CollapseSizes()

sizes_jeans <- sizes %>%
  filter(market == "Women") %>%
  filter(size %in% sizes_jeans_vec) %>%
  filter(category %in% c("Jeans", "Pants & Jumpsuits", "Shorts")) %>%
  CollapseSizes()





  
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


##








# lululemon vs all other athletic pants 
pants_sales <- dbGetQuery(con, "SELECT date_sold, market, category, SUM(price), count(item_id) FROM solds WHERE category = 'Pants & Jumpsuits' GROUP BY date_sold, market, category")
lulu_sales  <- dbGetQuery(con, "SELECT date_sold, market, category, brand, SUM(price), count(item_id) FROM solds WHERE category = 'Pants & Jumpsuits' AND brand = 'lululemon athletica' GROUP BY date_sold, market, category, brand")

pants_sales %<>% rename(sum_all = sum, count_all = count)
lulu_sales %<>% select(date_sold, sum, count) %>% rename(sum_lulu = sum, count_lulu = count)
pants_sales %<>% left_join(lulu_sales, by = "date_sold")






