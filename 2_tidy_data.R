library(tidytext)
data(stop_words)

# Custom stop words
custom_stop_words <- data.frame(word = c("miss", "flight", "tukish", 
                                         "airlines", "flights", 
                                         "airline", "turkish", "de"), 
                                lexicon = c("custom")) %>% rbind(stop_words)


tokenize <- function(file, data_type) {
  data_tibble <- readRDS(file = file) %>% 
    tibble::as_tibble()
  
  data_vector <- data_tibble %>% 
    dplyr::pull(data_type) %>% 
    iconv(from = "UTF-8", to = "Latin1")
  
  tokens <- tibble::as_tibble(data_vector) %>% 
    dplyr::filter(!is.na(value)) %>% 
    dplyr::mutate(line = rownames(.)) %>%
    dplyr::select(line, value) %>% 
    tidytext::unnest_tokens(word, value)
  
  tokens
}

# Convert non_base verbs into regular

extract_non_base <- function(data) {
  data %>%
    dplyr::rename(non_base = word) %>%
    dplyr::left_join(readRDS("sahban_base_lexicon"), by = "non_base") %>% 
    dplyr::mutate(base = ifelse(is.na(base), non_base, base)) %>%
    dplyr::rename(word = base) %>%
    dplyr::select(-one_of("non_base")) 
}

facebook_tokens <- tokenize("300_posts_comments", "comments") %>% 
  extract_non_base()
tripadvisor_tokens <- tokenize("tripadvisor_turkishairlines6846.rds", "review") %>% 
  extract_non_base()

saveRDS(facebook_tokens, "facebook_tokens")
saveRDS(tripadvisor_tokens, "tripadvisor_tokens")


word_count <- function(data) {
  readRDS(data) %>% 
    dplyr::anti_join(custom_stop_words, by = "word") %>% 
    dplyr::count(word, sort = TRUE) %>% 
    tibble::as_tibble()
}

facebook_word_count <- word_count("facebook_tokens")
tripadvisor_word_count <- word_count("tripadvisor_tokens")

facebook_word_count
tripadvisor_word_count

saveRDS(facebook_word_count, "tokens_count_300")
saveRDS(tripadvisor_word_count, "TA_tokens_count_6846")
