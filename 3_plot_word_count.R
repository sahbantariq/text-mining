library(ggplot2)


word_count_plot <- function(data, min_count) {
  tokens_count <- readRDS(data)
  
  tokens_count %>% 
    dplyr::anti_join(custom_stop_words) %>% 
    dplyr::filter(n > min_count) %>%
    dplyr::mutate(word = reorder(word, n)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(word, n)) +
    ggplot2::geom_col() +
    ggplot2::xlab(NULL) + 
    ggplot2::coord_flip()
}

facebook_plot <- word_count_plot("tokens_count_300", 25)
tripadvisor_plot <- word_count_plot("TA_tokens_count_6846", 250)

