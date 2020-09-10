library(tidymodels)
library(magrittr)
library(stringr)


# Preliminaries
setwd("C:/Users/maran/Dropbox/Web Scraping")
rm(list = ls())
gc()


# Load data
posh_sales_original <- readRDS("./inter/modeling/ready_data_2020-08.RDS")

set.seed(42)
posh_sales <- slice_sample(posh_sales_original, n = 50000)

rm(posh_sales_original)
gc()

# Split into training and testing data
set.seed(42)
posh_split <- initial_split(posh_sales)
gc()

# Process data
posh_recipe <- training(posh_split) %>%
  recipe(price ~ market + category + subcategory + brand_mod + size_mod + nwt + boutique) %>%
  step_string2factor(market, category, subcategory, brand_mod, size_mod) %>%
  prep()
gc()

# Get processed training and testing data
posh_training <- juice(posh_recipe)
posh_testing  <- posh_recipe %>% bake(testing(posh_split))


# Model Training
gc()
posh_reg <- linear_reg() %>%
  set_engine("lm") %>%
  fit(price ~ market + category + subcategory + brand_mod + size_mod + nwt + boutique, data = posh_training) %>%
  fit(price ~ market + category + subcategory + brand_mod  + nwt + boutique, data = posh_training)
gc()


tested_data <- posh_reg %>% 
  predict(posh_testing) %>% 
  bind_cols(posh_testing)

metrics(tested_data, truth = price, estimate = .pred)

tested_data %<>% mutate(error = abs(price - .pred))
