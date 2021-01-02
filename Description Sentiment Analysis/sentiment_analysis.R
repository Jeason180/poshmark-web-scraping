library(tidyverse)
library(magrittr)
library(lubridate)
library(tidytext)


# Preliminaries
setwd("C:/Users/maran/Documents/Data Projects/Web Scraping/Scraped datasets")
rm(list = ls())
gc()

# load compiled data
load("./sentiment/madewell_compiled.RDa")



# lexicons
lex_nrc <- get_sentiments("nrc")
lex_bing <- get_sentiments("bing")
lex_afinn <- get_sentiments("afinn")

lex_nrc %<>% mutate(present = 1)
lex_nrc_wide <- pivot_wider(lex_nrc, id_cols = word, names_from = sentiment, 
                            names_prefix = "nrc_", values_from = present, values_fill = 0) %>%
  mutate(nrc_any = 1)

lex_bing %<>% mutate(present = 1)
lex_bing_wide <- pivot_wider(lex_bing, id_cols = word, names_from = sentiment, 
                            names_prefix = "bing_", values_from = present, values_fill = 0) %>%
  mutate(bing_any = 1)

lex_afinn %<>% rename(afinn_value = value) %>%
  mutate(afinn_any = 1)

lex_all <- full_join(lex_nrc_wide, lex_bing_wide, by = "word")
lex_all <- full_join(lex_all, lex_afinn, by = "word")
lex_all %<>% mutate(match = 1)

rm(lex_afinn, lex_bing, lex_bing_wide, lex_nrc, lex_nrc_wide)


# transform into data frame of words
closet %<>% mutate(desc_title = paste(title, description))

words_tidy <- closet %>%
  select(item_id, title, desc_title) %>%
  unnest_tokens(word, desc_title) 



words_lex <- left_join(words_tidy, lex_all, by = "word")

match_table <- words_lex %>% 
  filter(match == 1) %>%
  group_by(word) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

nomatch_table <- words_lex %>% 
  filter(is.na(match)) %>%
  group_by(word) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

# Remove words that have a particular meaning in this context
remove_words <- c("skinny", "black", "blue", "distressed", "distressing")

# Summarize number of words in each category
words_summary <- words_lex %>%
  filter(!(word %in% remove_words)) %>%
  group_by(item_id, title) %>%
  summarize(n_words = n(),
            matched_words = sum(match, na.rm = T),
            across(starts_with("nrc"), ~sum(.x, na.rm = T)),
            across(starts_with("bing"), ~sum(.x, na.rm = T)),
            across(starts_with("afinn"), ~sum(.x, na.rm = T)))

words_summary %<>% mutate(nrc_net = nrc_positive - nrc_negative,
                          bing_net = bing_positive - bing_negative)


closet_short <- closet %>% 
  select(item_id, title, user, subcategory, price, 
         nwt, boutique, date_posted, date_sold) %>%
  mutate(days_to_sell = date_sold - date_posted)

words_combined <- left_join(closet_short, words_summary, by = c("item_id", "title"))

save(words_combined, file = "./sentiment/madewell_sentiment_combined.RDa")
write.csv(words_combined, "./sentiment/madewell_sentiment_analysis.csv", row.names = F, na = "")





# graphs
sentiment_vars <- grep("nrc|afinn|bing", names(words_combined), value = T) 
sentiment_vars %<>% setdiff(c("nrc_negative", "nrc_positive", "nrc_any", "bing_negative", "bing_positive", "bing_any", "afinn_any"))
sentiment_vars <- c(sentiment_vars, "n_words")

outcomes <- c("avg_price", "avg_days")


for (s_var in sentiment_vars) {
  for (o_var in outcomes) {
    sentiment_var <- s_var
    outcome_var <- o_var

    
    if(o_var == "avg_price") formula <- paste("price ~", sentiment_var)
    if(o_var == "avg_days") formula <- paste("days_to_sell ~", sentiment_var)
    
    reg_result <- lm(formula, data = words_combined)

    result_correlation <- words_combined %>%
      group_by(.data[[sentiment_var]]) %>%
      summarize(
        avg_price = mean(price),
        avg_days = mean(days_to_sell, na.rm = T),
        med_days = median(days_to_sell, na.rm = T),
        count = n()
      )

    title <- paste0("Effect of ", sentiment_var, " on ", outcome_var, ", beta = ", round(reg_result$coefficients[2], digits = 3))

    bubble_plot <- ggplot(result_correlation, aes(x = .data[[sentiment_var]], y = .data[[outcome_var]], size = count)) +
      geom_point(alpha = 0.5) +
      scale_size(range = c(1, 20), name = "Number of Items") +
      geom_abline(
        intercept = reg_result$coefficients[1], slope = reg_result$coefficients[2],
        color = "blue", size = 1
      ) +
      ggtitle(title)

    filename <- paste0("graph_", sentiment_var, "_", outcome_var)

    ggsave(paste0("C:/Users/maran/Dropbox/UMD/2nd Year/Econ 664_Industrial Organization/Final Proposal/results/", filename, ".png"), plot = bubble_plot)
  }
}