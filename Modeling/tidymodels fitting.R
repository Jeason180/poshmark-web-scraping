library(tidymodels)
library(tidyverse)
library(magrittr)
library(glmnet)



# Preliminaries
setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()


# Load data
posh_sales <- readRDS("./modeling/ready_data_2020-11.RDS")



# Split into training and testing data
set.seed(42)
posh_split <- initial_split(posh_sales)
gc()

# Process data
posh_recipe <- training(posh_split) %>%
  recipe(price ~ super_category + brand_mod + size_mod + nwt + boutique + month_sold) %>%
  step_dummy(all_nominal()) %>%
  prep()
gc()

# Get processed training and testing data
posh_training <- juice(posh_recipe)
posh_testing  <- posh_recipe %>% bake(testing(posh_split))


# Model Training
gc()
posh_reg <- linear_reg(penalty = 0) %>%
  set_engine("glmnet") %>%
  fit(price ~ ., data = posh_training)
  #fit(price ~ market + category + subcategory + brand_mod  + nwt + boutique, data = posh_training)
gc()


tested_data <- posh_reg %>% 
  predict(posh_testing) %>%
  bind_cols(posh_testing)

metrics(tested_data, truth = price, estimate = .pred)

tested_data %<>% mutate(error = abs(price - .pred))
