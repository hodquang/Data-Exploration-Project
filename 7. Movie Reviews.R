imdb <- read.csv("7.1 HP_123.csv")
# Treat cap and non-cal the same
imdb <- imdb %>% mutate(Narrative_lower = tolower(review))
imdb <- imdb %>% mutate(Narrative_noNumbers = gsub('[[:digit:]]','',Narrative_lower))
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
imdb <- imdb %>% mutate(Narrative_noStopWords = gsub(stopwords_regex,'',Narrative_noNumbers))
imdb <- imdb %>% mutate(Narrative_noPunctuation = gsub('[[:punct:]]','',Narrative_noStopWords))
imdb <- imdb %>% mutate(Narrative_noTypos = gsub('thankssssssss','thanks',Narrative_noPunctuation))
imdb <- imdb %>% mutate(Narrative_noSpaces = gsub('\\s+',' ',Narrative_noTypos))
imdb <- imdb %>% mutate(Narrative_Lemma = lemmatize_strings(Narrative_noSpaces))
imdb <- imdb %>%
  select(rating=rating, review = Narrative_Lemma) %>%
  filter(review != "")
imdb <- imdb %>% filter(rating != "NaN")

wordcloud(imdb$review, scale=c(4,0.5), max.words=500, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

new_nodes$x <- as.character(new_nodes$x)
want_list <- new_nodes %>% unnest_tokens(word, x) %>% group_by(word)
wantlist <- c(want_list$word)
`%notin%` <- Negate(`%in%`)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(stringr)
  
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    
    
    word.list = str_split(sentence, '\\s+')
    
    words = unlist(word.list)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score=sum(pos.matches) - sum(neg.matches)
  }, pos.words, neg.words, .progress=.progress )
  
  return(scores)
}

imdb$result <- score.sentiment(imdb$review,pos.words,neg.words)
imdb$sentiment=ifelse(imdb$result==0,"Neutral",ifelse(imdb$result>0,"Positive","Negative"))

ggplot(imdb, aes(x=result, y=reorder(rating), colour=sentiment)) +
  geom_segment(aes(x=result, xend=result, yend = rating), size=1.1, alpha=0.8) +
  #geom_bar(stat = "identity", fill= 1) +
  geom_point(size=1.0) +
  ylab("Rating") +
  xlab("Sentiment Scores") +
  ggtitle("Reviews Sentiment Score by Rating for HP, The CoS") + 
  theme(legend.position="none") +
  #facet_wrap(~sentiment, ncol=4) +
  coord_flip()


y <- imdb %>%
  unnest_tokens(word, review) %>% 
  group_by(word) %>%
  filter(word %in% c(wantlist, "Voldermort")) %>%
  filter(word %notin% c("everyone", "short", "age", "professor", "young", "harry","gilderoy","tom"))


y %>%
  summarize(total= n()) %>%
  count(word, wt = total) %>%
  ungroup() %>%
  filter(n > 50) %>%
  mutate(term = reorder(word, n)) %>%
  ggplot(aes(term, n, fill = -n)) +
  geom_bar(stat = "identity") +
  ylab("Contribution by word mentioned") +
  coord_flip()


y %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "orange", "blue"),
                   max.words = 2000)


##### Bing Sentiment
imdb_bing <- imdb %>%
  unnest_tokens(word, review) %>%  
  group_by(word) %>%
  summarize(total= n()) %>%
  inner_join(get_sentiments("bing"), by=c("word" = "word")) %>%
  ungroup()

imdb_bing %>%
  count(sentiment, word, wt = total) %>%
  ungroup() %>%
  filter(n > 50) %>%
  #filter(word %notin% nodes) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(word, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

#### NRC Sentiment
imdb_nrc <- imdb %>% select(rating = rating, review = review, result = result)
imdb_nrc <- imdb_nrc %>%
  unnest_tokens(word, review) %>%                           
  inner_join(nrc_data, by=c("word"="term"))  %>%
  group_by(word, sentiment) %>%
  summarize(total= n()) %>%
  ungroup()

imdb_nrc %>%
  count(sentiment, word, wt = total) %>%
  ungroup() %>%
  filter(n > 150) %>%
  #mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(word, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

