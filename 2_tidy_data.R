library(tidytext)
data(stop_words)

# Custom stop words
custom_stop_words <- data_frame(word = c("miss"), lexicon = c("custom")) %>% 
  bind_rows(stop_words)


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
  dplyr::anti_join(stop_words, by = "word") %>% 
  dplyr::count(word, sort = TRUE)

saveRDS(tokens_count, "tokens_count_300")


# Add date and post-number columns
