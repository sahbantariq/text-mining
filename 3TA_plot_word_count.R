library(ggplot2)

tokens_count <- readRDS("TA_tokens_count_6846")

tokens_count %>% 
  dplyr::filter(n > 250) %>%
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(word, n)) +
  ggplot2::geom_col() +
  ggplot2::xlab(NULL) + 
  ggplot2::coord_flip()




