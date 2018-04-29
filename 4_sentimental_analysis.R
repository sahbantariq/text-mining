library(dplyr)
library(tidytext)
library(wordcloud)

comments <- readRDS(file = "300_posts_comments")

# Assigning numbers to posts and comments of each post
mutated_comments <- comments %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(comments = iconv(.$comments, from = "UTF-8", to = "Latin1")) %>% 
  dplyr::mutate(post_number = as.numeric(factor(id))) %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(comment_number = row_number()) %>% 
  dplyr::select(comments, id, post_number, comment_number, created_time, type, 
                likes_count, comments_count, shares_count) 


# Tokenize the above dataframe with comment numbers and post numbers
tidy_comments <- mutated_comments %>% 
  tidytext::unnest_tokens(word, comments)


comment_sentiments <- function(lexicon, group_by = sentiment) {
  tidy_comments %>% 
    dplyr::inner_join(get_sentiments(lexicon), by = "word") %>% 
    dplyr::count(post_number, sentiment) %>% 
    tidyr::spread(sentiment, n, fill = 0) %>% 
    dplyr::mutate(sentiment = positive - negative) 
}

# Bing
comment_sentiments_bing <- comment_sentiments("bing")

# nrc
comments_sentiments_nrc <- comment_sentiments("nrc")

#afinn
comments_sentiments_afinn <- tidy_comments %>% 
  dplyr::inner_join(tidytext::get_sentiments("afinn"), by = "word") %>% 
  dplyr::group_by(post_number) %>% 
  dplyr::summarise(score = sum(score)) 

# Most common positive and negative words
bing_word_counts <- tidy_comments %>% 
  dplyr::inner_join(tidytext::get_sentiments("bing")) %>% 
  dplyr::count(word, sentiment, sort = TRUE) %>% 
  dplyr::ungroup()

# Plot bing word counts
bing_word_counts %>% 
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


# Wordcloud
library(wordcloud)

tidy_comments %>%
  dplyr::anti_join(tidytext::stop_words) %>%
  dplyr::count(word) %>%
  with(wordcloud::wordcloud(word, n, max.words = 50))

# Comparison Cloud
library(reshape2)

tidy_comments %>%
  dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                              max.words = 100)

# Looking at units beyond just words

# The tidytext package only tokenize the input into sentence if the input is in 
# text. In our data, there are many other symbols used in the comments. 
# Therefore, "sentences" token cannot be used. In order to remove symbols and 
# other elements that are not included in english language, we have to use a 
# different package, i.e. hunspell 

# English Dictionary
qdapDictionaries::DICTIONARY[,1]

en_word_comments <- mutated_comments %>% 
  dplyr::ungroup() %>% 
  tidytext::unnest_tokens(words, comments) %>% 
  dplyr::filter(words %in% qdapDictionaries::DICTIONARY[,1])

en_word_sentence_comments <- en_word_comments %>% 
  dplyr::group_by(post_number, comment_number) %>% 
  dplyr::mutate(sentence = paste(words, collapse = " ")) %>%
  dplyr::distinct(sentence, .keep_all = TRUE) %>% 
  dplyr::as_data_frame() %>% 
  dplyr::mutate(sentence = iconv(sentence, to = 'latin1')) %>% 
  dplyr::ungroup()

# Sentence as tokens with post number and comment number

sentence_tokens <- en_word_sentence_comments %>% 
  dplyr::select(post_number, comment_number, sentence) %>%
  dplyr::ungroup() %>% 
  tidytext::unnest_tokens(sentences, sentence, token = "sentences")

## Get the Negative Words to All Words Ratio or Positive Words to All Words Ratio
# We can do it for trip advisor temporal data

bingnegative <- get_sentiments("bing") %>% 
  dplyr::filter(sentiment == "negative")

wordcounts <- tidy_comments %>% 
  dplyr::group_by(post_number) %>%
  dplyr::summarize(word = n())

tidy_comments %>%
  dplyr::semi_join(bingnegative) %>%
  dplyr::group_by(post_number) %>%
  dplyr::summarize(negativewords = n()) %>%
  dplyr::left_join(wordcounts, by = c("post_number")) %>%
  dplyr::mutate(ratio = negativewords/word) %>%
  dplyr::top_n(10) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(ratio))