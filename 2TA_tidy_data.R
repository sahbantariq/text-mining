library(tidytext)
data(stop_words)

# Custom stop words
custom_stop_words <- data.frame(word = c("miss"), lexicon = c("custom")) %>% 
  rbind(stop_words)


reviews <- readRDS(file = "tripadvisor_turkishairlines6846.rds")

comments_tibble <-  tibble::as_tibble(reviews)

comments_vector <- comments_tibble$review %>% 
  iconv(from = "UTF-8", to = "Latin1")

tokens <- tibble::as_tibble(comments_vector) %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::mutate(line = rownames(.)) %>% 
  dplyr::select(line, value) %>% 
  tidytext::unnest_tokens(word, value)

tokens_count <- tokens %>%
  dplyr::anti_join(stop_words, by = "word") %>% 
  dplyr::count(word, sort = TRUE)

saveRDS(tokens_count, "TA_tokens_count_6846")


# Add date and post-number columns
