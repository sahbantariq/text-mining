library(tidytext)
data(stop_words)

# Custom stop words
custom_stop_words <- data.frame(word = c("miss", "flight", "tukish", 
                                         "airlines", "flights", "airline",
                                         "turkish"), 
                                lexicon = c("custom")) %>%
  rbind(stop_words)


comments <- readRDS(file = "300_posts_comments")

comments_tibble <-  tibble::as_tibble(comments)

comments_vector <- comments_tibble$comments %>% 
  iconv(from = "UTF-8", to = "Latin1")

tokens <- tibble::as_tibble(comments_vector) %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::mutate(line = rownames(.)) %>% 
  dplyr::select(line, value) %>% 
  tidytext::unnest_tokens(word, value)

tokens_count <- tokens %>%
  dplyr::anti_join(custom_stop_words, by = "word") %>% 
  dplyr::count(word, sort = TRUE)

saveRDS(tokens_count, "tokens_count_300")


# Add date and post-number columns
