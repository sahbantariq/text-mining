library(ggplot2)

tokens_count <- readRDS("tokens_count_300")

tokens_count %>% 
  dplyr::filter(n > 25) %>%
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot2::ggplot(mapping = aes(word, n)) +
  ggplot2::geom_col() +
  ggplot2::xlab(NULL) + 
  ggplot2::coord_flip()
  
  
  

