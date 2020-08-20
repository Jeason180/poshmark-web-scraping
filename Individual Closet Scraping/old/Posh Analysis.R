library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(writexl)

rm(list = ls())
setwd("C:/Users/maran/Documents/Data Stuff/Web Scraping")
source("./code/Posh Scraping Functions.R")

# Mogi Beth
# Prep for analysis
load("./inter/individual closets/mogi_closet_20191212_1to1000.RDa")
load("./inter/individual closets/mogi_closet_20191212_1001toend.RDa")

mogi_closet <- bind_rows(mogi_closet_part1, mogi_closet_part2)
rm(mogi_closet_part1, mogi_closet_part2)

# Empty Hanger
load("./inter/individual closets/emptyhanger_closet_20191215.RDa")

# Nicole State
load("./inter/individual closets/nicole_closet_20191216.RDa")

# Becky Park
load("./inter/individual closets/beckypark_closet_20191223.RDa")


all_closets <- bind_rows(mogi_closet, emptyhanger_closet, nicole_closet, beckypark_closet)

# Clean up the dates:

all_closets$date_sold <- sapply(all_closets$item_id, DateSold, closet = all_closets) %>%
  as.Date(origin = "1970-01-01")

all_closets %<>% mutate(days_to_sell = as.numeric(date_sold - date_posted),
                        month_sold = floor_date(date_sold, "month"),
                        year_sold = year(date_sold),
                        month_posted = floor_date(date_posted, "month"))

save(all_closets, file = "./output/closets_scraped_201912.RDa")








load("./output/closets_scraped_201912.RDa")

#closet <- all_closets %>% filter(user == "emptyhanger")

# Sales by month and year
PoshAnalysis <- function(closet, userid = "all", year_of_interest = 2019){

if(userid != "all") closet <- closet %>% filter(user %in% userid)

monthly_sales <- closet %>%
  group_by(user, month_sold, year_sold) %>%
  filter(status == "Sold") %>%
  summarize(items_sold = n(),
            sales = sum(price)) %>%
  arrange(user, year_sold, month_sold) %>%
  mutate(asp = sales / items_sold)



yearly_sales <- closet %>%
  group_by(user, year_sold) %>%
  filter(status == "Sold") %>%
  summarize(items_sold = n(),
            sales = sum(price)) %>%
  arrange(user, year_sold) %>%
  mutate(asp = sales / items_sold)

# Top categories for 2019
by_category <- closet %>%
  group_by(user, category) %>%
  filter(status == "Sold" & year_sold == year_of_interest) %>%
  summarize(items_sold = n(),
            sales = sum(price)) %>%
  mutate(asp = sales/items_sold,
         pct_items = items_sold / sum(items_sold),
         pct_sales = sales / sum(sales))

# Top brands for 2019
top_brands <- closet %>%
  filter(status == "Sold" & year_sold == year_of_interest) %>%
  group_by(user, brand) %>%
  summarize(items_sold = n(),
            sales = sum(price)) %>%
  mutate(asp = sales/items_sold,
         pct_items = items_sold / sum(items_sold),
         pct_sales = sales / sum(sales)) %>%
  arrange(user, desc(sales)) %>%
  mutate(brand_rank = rank(-sales))

top5 <- top_brands %>% filter(!is.na(brand)) %>% top_n(5, sales) %>% pull(brand)
top20 <- top_brands %>% filter(!is.na(brand)) %>% top_n(20, sales) %>% pull(brand) %>% setdiff(top5)
top_brands$group <- "all other"
top_brands$group[top_brands$brand %in% top20] <- "top 20"
top_brands$group[top_brands$brand %in% top5] <- "top 5"
top_brands$group[is.na(top_brands$brand)] <- "unbranded"

# revenue by brand categories
brand_summary <- top_brands %>% 
  group_by(group) %>%
  summarize(sales = sum(sales)) %>%
  mutate(percent = sales / sum(top_brands$sales)*100)


rolling_closet <- closet %>%
  group_by(user, month_posted) %>%
  summarize(items_posted = n()) %>%
  full_join(monthly_sales, by = c("user" = "user", "month_posted" = "month_sold")) %>%
  rename(month = month_posted) %>%
  arrange(month) %>%
  mutate(opening_closet = NA, ending_closet = NA) %>%
  select(user, month, opening_closet, items_posted, items_sold, ending_closet)

rolling_closet$items_posted[is.na(rolling_closet$items_posted)] <- 0
rolling_closet$items_sold[is.na(rolling_closet$items_sold)] <- 0

for (i in 1:nrow(rolling_closet)){
  if(i == 1) {
    rolling_closet$opening_closet[i] <- 0
  } else {
    rolling_closet$opening_closet[i] <- rolling_closet$ending_closet[i-1]
  }
  rolling_closet$ending_closet[i] <- rolling_closet$opening_closet[i] + 
    rolling_closet$items_posted[i] -
    rolling_closet$items_sold[i]
}

rolling_closet %<>% mutate(items_available = opening_closet + items_posted,
                           sales_pct = items_sold / items_available * 100)


# Items listed per week
closet$year_posted <- year(closet$date_posted)
closet %<>% mutate(week_posted = strftime(date_posted, format = "%V") %>% as.numeric,
                   year_week_posted = paste0(year_posted, "_", week_posted))
items_listed <- closet %>% 
  group_by(user, year_week_posted, year_posted, week_posted) %>%
  summarize(count = n()) %>%
  arrange(user, year_posted, week_posted)


# Graphs
sales_graph <- ggplot(data = monthly_sales, aes(x = month_sold)) +
  geom_line(aes(y = sales)) + 
  ggtitle("Monthly Sales", subtitle = userid)

asp_graph <- ggplot(data = monthly_sales, aes(x = month_sold)) +
  geom_line(aes(y = asp), color = "red") + 
  ggtitle("Average Sales Price", subtitle = userid)

closet_graph <- ggplot(data = rolling_closet, aes(x = month)) +
  geom_line(aes(y = opening_closet), color = "red") + 
  ggtitle("Closet Size", subtitle = userid)

result_list <- list(closet = closet,
                    monthly_sales = monthly_sales, 
                    yearly_sales = yearly_sales,
                    by_category = by_category, 
                    top_brand = top_brands, 
                    brand_summary = brand_summary, 
                    items_listed = items_listed,
                    rolling_closet = rolling_closet, 
                    sales_graph, asp_graph, closet_graph)

return(result_list)
}


# 1 Closet
# 2 Monthly sales
# 3 Yearly sales
# 4 Sales by category
# 5 Top Brands
# 6 Brand breakdown by top 5 and 20
# 7 Items listed per week
# 8 Rolling closet
# 9 Sales graph
# 10 ASP graph
# 11 Closet size graph

emptyhanger_results <- PoshAnalysis(all_closets, userid = "emptyhanger")
mogi_results <- PoshAnalysis(all_closets, userid = "mogibeth")
nicole_results <- PoshAnalysis(all_closets, userid = "nicolestate")
beckypark_results <- PoshAnalysis(all_closets, userid = "beckypark")

write_xlsx(mogi_results[1:8], "output/mogi_results.xlsx")
write_xlsx(emptyhanger_results[1:8], "output/emptyhanger_results.xlsx")
write_xlsx(beckypark_results[1:8], "output/beckypark_results.xlsx")
write_xlsx(nicole_results[1:8], "output/nicole_results.xlsx")



