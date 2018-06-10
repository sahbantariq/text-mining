library(dplyr)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(gdapDictionaries)


review <- readRDS(file = "tripadvisor_turkishairlines6846.rds")

custom_stop_words <- data.frame(word = c("miss", "flight", "tukish", 
                                         "airlines", "flights", 
                                         "airline", "turkish", "de"), 
                                lexicon = c("custom")) %>%
  rbind(stop_words)

# Assigning numbers to use responses, i.e. posts, comments and reviews. 

numbered_response_tokens <- function(file, response_type) {
  
  dataset <-  readRDS(file = file)
  
  dataset %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(response_type = 
                    iconv(pull(., response_type), 
                          from = "UTF-8", to = "Latin1")) %>% 
    dplyr::mutate(post_number = as.numeric(factor(id))) %>% 
    dplyr::group_by(id) %>% 
    dplyr::mutate_if(is.factor, as.character)
}

# Convert non_base verbs into base verbs

extract_non_base <- function(data) {
  data %>%
    dplyr::rename(non_base = word) %>%
    dplyr::left_join(readRDS("sahban_base_lexicon"), by = "non_base") %>%
    dplyr::mutate(base = ifelse(is.na(base), non_base, base)) %>%
    dplyr::rename(word = base) %>%
    dplyr::select(-one_of("non_base")) 
}

# Convert plural nouns into singular nouns

extract_plural <- function(data) {
  data %>% 
    dplyr::rename(plural = word) %>% 
    dplyr::left_join(readRDS("sahban_noun_lexicon"), by = "plural") %>%
    dplyr::mutate(noun = ifelse(is.na(noun), plural, noun)) %>%
    dplyr::rename(word = noun) %>% 
    dplyr::select(-one_of("plural"))
}



tidy_response_facebook <- 
  numbered_response_tokens("300_posts_comments", "comments") %>% 
  dplyr::mutate(response_number = row_number()) %>% 
  dplyr::select(id, post_number, response_number, 
                comments, created_time, type, likes_count, comments_count, 
                shares_count) %>% 
  tidytext::unnest_tokens(word, comments) %>% 
  dplyr::anti_join(custom_stop_words) %>% 
  extract_non_base() %>% 
  extract_plural()


tidy_response_tripadvisor <- 
  numbered_response_tokens("tripadvisor_turkishairlines6846.rds", "review") %>% 
  dplyr::select(id, response_number = post_number, review, quote, rating, date) %>% 
  tidytext::unnest_tokens(word, review) %>% 
  dplyr::anti_join(custom_stop_words) %>% 
  extract_non_base() %>% 
  extract_plural()


# Adding words manually to the lexicon
sahban_base_lexicon <- readRDS("sahban_base_lexicon") %>% 
  dplyr::bind_rows(data.frame(base = c("travel", "travel"), 
                              non_base = c("travelled", "travelling")))
saveRDS(sahban_base_lexicon, "sahban_base_lexicon")

# Response sentiments

response_sentiments <- function(data, lexicon, group_by = sentiment) {
  data %>% 
    dplyr::inner_join(get_sentiments(lexicon), by = "word") %>%
    dplyr::count(response_number, sentiment) %>% 
    tidyr::spread(sentiment, n, fill = 0) %>% 
    dplyr::mutate(sentiment = positive - negative) 
}

# Bing
facebook_sentiments_bing <- response_sentiments(tidy_response_facebook, "bing")
tripadvisor_sentiments_bing <- response_sentiments(tidy_response_tripadvisor, "bing")

# nrc
facebook_sentiments_nrc <- response_sentiments(tidy_response_facebook, "nrc")
tripadvisor_sentiments_nrc <- response_sentiments(tidy_response_tripadvisor, "nrc")

#afinn
sentiments_afinn <- function(data) {
  data %>% 
    dplyr::inner_join(tidytext::get_sentiments("afinn"), by = "word") %>% 
    dplyr::group_by(id, response_number) %>% 
    dplyr::summarise(score = sum(score)) 
}

facebook_sentiments_afinn <- sentiments_afinn(tidy_response_facebook)
tripadvisor_sentiments_afinn <- sentiments_afinn(tidy_response_tripadvisor)

# Most common positive and negative words

frequent_sentiments <- function(data) {
  data %>% 
    dplyr::inner_join(tidytext::get_sentiments("bing")) %>% 
    dplyr::ungroup() %>% 
    dplyr::count(word, sentiment, sort = TRUE)
}

facebook_frequent_sentiments <- frequent_sentiments(tidy_response_facebook)
tripadvisor_frequent_sentiments <- frequent_sentiments(tidy_response_tripadvisor)


# Plot frequent sentiments counts

plot_sentiment_count <- function(data) {
  data %>% 
    dplyr::group_by(sentiment) %>% 
    dplyr::top_n(10) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(word = reorder(word, n)) %>% 
    ggplot2::ggplot(ggplot2::aes(word, n, fill = sentiment)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~sentiment, scales = "free_y") +
    ggplot2::labs(y = "Contribution to sentiment",
                  x = NULL) +
    ggplot2::coord_flip()
}

facebook_plot_sentiment_count <- 
  plot_sentiment_count(facebook_frequent_sentiments)
tripadvisor_plot_sentiment_count <- 
  plot_sentiment_count(tripadvisor_frequent_sentiments)


# WordCloudlibrary(wordcloud)

library(wordcloud)
word_cloud <- function(data, max_words) {
  data %>%
    dplyr::ungroup() %>% 
    dplyr::count(word) %>%
    with(wordcloud::wordcloud(word, n, max.words = 50))
}

facebook_word_cloud <- word_cloud(tidy_response_facebook, max_words = 50)
tripadvisor_word_cloud <- word_cloud(tidy_response_tripadvisor, max_words = 75)

# Sentiment Cloud
library(reshape2)

sentiment_cloud <- function(dataset) {
  dataset %>%
    dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
    dplyr::count(word, sentiment, sort = TRUE) %>%
    reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    wordcloud::comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                                max.words = 100)
}

facebook_sentiment_cloud <- sentiment_cloud(tidy_response_facebook)
tripadvisor_sentiment_cloud <- sentiment_cloud(tidy_response_tripadvisor)

# Get the Negative Words to All Words Ratio or Positive Words to All Words Ratio

sentiment_token_ratio <- function(data, sentiment_type = "negative") {
  
  negative_sentiment <- get_sentiments("bing") %>% 
    dplyr::filter(sentiment == sentiment_type)
  
  wordcounts <- data %>% 
    dplyr::group_by(response_number) %>%
    dplyr::summarize(word = n())
  
  data %>%
    dplyr::ungroup() %>% 
    dplyr::semi_join(negative_sentiment) %>%
    dplyr::group_by(id, response_number) %>%
    dplyr::summarize(negativewords = n()) %>%
    dplyr::left_join(wordcounts, by = c("response_number")) %>%
    dplyr::mutate(ratio = negativewords/word) %>%
    dplyr::top_n(10) %>%
    dplyr::ungroup() %>% 
    dplyr::arrange(desc(ratio))
  
}

facebook_negative_sentiment_ratio <- 
  sentiment_token_ratio(tidy_response_facebook, "negative")
tripadvisor_negative_sentiment_ratio <- 
  sentiment_token_ratio(tidy_response_tripadvisor, "negative")

facebook_positive_sentiment_ratio <- 
  sentiment_token_ratio(tidy_response_facebook, "positive")
tripadvisor_positive_sentiment_ratio <- 
  sentiment_token_ratio(tidy_response_tripadvisor, "positive")


# Looking at units beyond just words

# The tidytext package only tokenize the input into sentence if the input is in 
# text. In our data, there are many other symbols used in the review. 
# Therefore, "sentences" token cannot be used. In order to remove symbols and 
# other elements that are not included in english language, we have to use a 
# different package, i.e. hunspell 

untidy_response_facebook <- 
  numbered_response_tokens("300_posts_comments", "comments") %>% 
  dplyr::mutate(comment_number = row_number()) %>% 
  dplyr::select(id, post_number, comment_number, comments, 
                created_time, type, likes_count, comments_count, 
                shares_count)

untidy_response_tripadvisor <- 
  numbered_response_tokens("tripadvisor_turkishairlines6846.rds", "review") %>% 
  dplyr::select(id, post_number, review, quote, rating, date)


sentence_tokens <- function(data = untidy_response_facebook, 
                            response_column = "comments", 
                            group_by = "comment_number") {
  # English Dictionary
  qdapDictionaries::DICTIONARY[,1]
  
  en_word_comments <- data %>% 
    dplyr::ungroup() %>% 
    tidytext::unnest_tokens_("word", response_column) %>% 
    dplyr::filter(word %in% qdapDictionaries::DICTIONARY[,1])
  
  en_word_sentence_comments <- en_word_comments %>% 
    dplyr::group_by_("id", group_by) %>% 
    dplyr::mutate(sentence = paste(word, collapse = " ")) %>%
    dplyr::distinct(sentence, .keep_all = TRUE) %>% 
    dplyr::as_data_frame() %>% 
    dplyr::mutate(sentence = iconv(sentence, to = 'latin1')) %>% 
    dplyr::ungroup()
  
  # Sentence as tokens with post number and comment number
  
  en_word_sentence_comments %>% 
    dplyr::select_("id", group_by, "sentence") %>%
    dplyr::ungroup() %>% 
    tidytext::unnest_tokens(sentences, sentence, token = "sentences")
}

facebook_sentence_tokens <- 
  sentence_tokens(data = untidy_response_facebook,
                  response_column = "comments",
                  group_by = "comment_number")

tripadvisor_sentence_tokens <- 
  sentence_tokens(data = untidy_response_tripadvisor,
                  response_column = "review",
                  group_by = "post_number")







