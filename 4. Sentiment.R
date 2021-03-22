# install.packages("tidyr")
# install.packages("textdata")
# install.packages("gplots")
# install.packages("reshape2")

library(tidyr)
library(textdata)
library(gplots)
library(reshape2)


nrc_data = lexicon::nrc_emotions %>%  
  gather("sentiment", "flag", anger:trust, -term) %>% 
  filter(flag==1)

##### Bing Sentiment
scripts_bing <- scripts %>%
  unnest_tokens(word, Narrative) %>%  
  group_by(word) %>%
  summarize(total= n()) %>%
  inner_join(get_sentiments("bing"), by=c("word" = "word")) %>%
  ungroup()

scripts_bing %>%
  count(sentiment, word, wt = total) %>%
  ungroup() %>%
  filter(n > 6) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(word, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

scripts_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 2000)
  


##### emotions word to total words
emotion_words_count <- scripts %>% select(ID = ID, Character1 = Character1, Character2 = Character2, Narrative = Narrative)

emotion_words_count <- emotion_words_count %>% 
  unnest_tokens(word, Narrative) %>%                           
  inner_join(nrc_data, by=c("word"="term"))  %>%
  group_by(Character1, sentiment) %>%
  summarize(emotion= n()) %>%
  ungroup()

emotions_to_total_words <- total_words_count %>%
  left_join(emotion_words_count, by="Character1") %>%
  mutate(percent_emotions=round((emotion/total)*100,1))


### pull emotion words and aggregate by Character1 and emotion terms
emotions <- scripts %>% select(ID = ID, Character1 = Character1, Character2 = Character2, Narrative = Narrative)
  

emotions <- emotions %>% 
  unnest_tokens(word, Narrative) %>%                           
  inner_join(nrc_data, by=c("word"="term"))  %>%
  group_by(Character1, sentiment) %>%
  summarize( freq = n()) %>%
  mutate(percent=round(freq/sum(freq)*100)) %>%
  select(-freq) %>%
  filter(percent != 100) %>%
  filter(Character1 %in% c("Harry", "Tom riddle", "Hagrid", "Dumbledore", "Draco malfoy", "Dobby", "Ron","Gilderoy lockhart", "Hermione")) %>%
  ungroup()


### need to convert the data structure to a wide format
emo_box = emotions %>%
  spread(sentiment, percent, fill=0) %>%
  ungroup()

boxplot2(emo_box[,c(2:9),], col="lightblue", lty=1, shrink=0.8, textcolor="red", xlab="Emotion Terms", ylab="Emotion words count (%)", main="Distribution of emotion words count")



scripts_afinn <- scripts %>%
  unnest_tokens(word, Narrative) %>%  
  group_by(word, ID, Character1) %>%
  summarize(total= n()) %>%
  inner_join(get_sentiments(lexicon = "afinn"), by=c("word" = "word")) %>%
  filter(Character1 %in% c("Harry", "Tom riddle")) %>%
  filter(ID %in% c(950:1025)) %>%
  ungroup()


### calculate overall averages and standard deviations for each emotion term
overall_mean_sd <- emotions %>%
  group_by(sentiment) %>%
  summarize(overall_mean=mean(percent), sd=sd(percent))
### draw a bar graph with error bars
ggplot(overall_mean_sd, aes(x = reorder(sentiment, -overall_mean), y=overall_mean)) +
  geom_bar(stat="identity", fill="darkgreen", alpha=0.7) + 
  geom_errorbar(aes(ymin=overall_mean-sd, ymax=overall_mean+sd), width=0.2,position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0, 40)) +
  xlab("Emotion Terms") +
  ylab("Emotion words count (%)") +
  ggtitle("Emotion words expressed in Movie 2") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip( )


## Hi / Low plots compared to the average
emotions_diff <- emotions  %>%
  left_join(overall_mean_sd, by="sentiment") %>%
  mutate(difference=percent-overall_mean)
  #filter(sentiment %in% c("anger"))

ggplot(emotions_diff, aes(x=Character1, y=difference, colour=difference>0)) +
  geom_segment(aes(x=Character1, xend=Character1, y=0, yend=difference), size=1.1, alpha=0.8) +
  geom_point(size=1.0) +
  ylab("Net emotion words count (%)") +
  xlab("Characters") +
  ggtitle("Emotion words expressed in HP Movie 2") + 
  theme(legend.position="none") +
  facet_wrap(~sentiment, ncol=4) +
  coord_flip()

