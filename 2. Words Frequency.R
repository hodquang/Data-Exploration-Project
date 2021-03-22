# isntall.packages("tidytext")
# install.packages("ggplot2")
# install.packages("ggraph")

library(tidytext)
library(ggplot2)
library(ggraph)


# Words most spoken by Characters
scripts_words <- scripts %>%
  unnest_tokens(word, Narrative) %>%
  count(Character1, word, sort = TRUE)

total_words_count <- scripts %>%
  unnest_tokens(word, Narrative) %>%  
  group_by(Character1) %>%
  summarize(total= n()) %>%
  ungroup()
 

scripts_words <- left_join(total_words_count, scripts_words)
scripts_words <- scripts_words %>%
  bind_tf_idf(word, Character1, n) %>%
  filter(total > 200)

scripts_words %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Character1) %>% 
  top_n(4, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = Character1)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Character1, ncol = 2, scales = "free") +
  coord_flip()

# Bi-gram words spoken by Characters
bigrams_filtered <- scripts %>%
  mutate(Narrative_noSpace = gsub('  ','',Narrative)) %>%
  unnest_tokens(bigram, Narrative_noSpace, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 != 'NA') %>%
  filter(word2 != 'NA')

bigrams_count <- bigrams_filtered %>%
  count(word1, word2, sort=TRUE)

bigrams_count <- bigrams_count %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(n>5)

want_list <- c(bigrams_count$bigram)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united <- bigrams_united %>%
  filter(bigram %in% want_list)

bigram_tf_idf <- bigrams_united %>%
  count(Character1, bigram) %>%
  bind_tf_idf(bigram, Character1, n) %>%
  arrange(desc(tf_idf)) %>%
  filter(n>1)

bigram_tf_idf %>%
  mutate(word = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(Character1) %>%
  top_n(3, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(bigram, tf_idf, fill = Character1)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Character1, ncol = 2, scales = "free") +
  coord_flip()


bigrams_count <- bigrams_filtered %>%
  count(word1, word2, sort=TRUE)
bigram_graph <- bigrams_count %>% filter(n>2) %>% graph_from_data_frame(directed = TRUE)


a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
