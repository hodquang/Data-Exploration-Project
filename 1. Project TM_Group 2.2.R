# install.packages("dplR")
# install.packages("textstem")
# install.packages("tm")
# install.packages('wordcloud')
# install.packages ("RColorBrewer")

library(plyr)
library(dplyr)
library(R.utils)
library(textstem)
library(tm)
library(wordcloud)
library(RColorBrewer)

setwd("C:/Users/USER/Desktop/MIS Project")
setwd("/Users/quangthefrog/Downloads/MIS Project")
scripts <- read.csv("1.1 Scripts_sentiment.csv")

######### Text Pre-Processing #######

######### Character 1 #########
# Treat cap and non-cal the same
scripts <- scripts %>% mutate(Narrative_lower = tolower(Character1))
scripts <- scripts %>% mutate(Narrative_upper = capitalize(Narrative_lower))
scripts <- scripts %>% mutate(Character1_Lemma = lemmatize_strings(Narrative_upper))

########## Character 2 ############
scripts <- scripts %>% mutate(Narrative_lower = tolower(Character2))
scripts <- scripts %>% mutate(Narrative_upper = capitalize(Narrative_lower))
scripts <- scripts %>% mutate(Character2_Lemma = lemmatize_strings(Narrative_upper))

######### Narrative ###########
scripts <- scripts %>% mutate(Narrative_lower = tolower(Narrative))
scripts <- scripts %>% mutate(Narrative_noNumbers = gsub('[[:digit:]]','',Narrative_lower))
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
scripts <- scripts %>% mutate(Narrative_noStopWords = gsub(stopwords_regex,'',Narrative_noNumbers))
scripts <- scripts %>% mutate(Narrative_noPunctuation = gsub('[[:punct:]]','',Narrative_noStopWords))
scripts <-scripts %>% mutate(Narrative_noTypos = gsub('thankssssssss','thanks',Narrative_noPunctuation))
scripts <- scripts %>% mutate(Narrative_noSpaces = gsub('\\s+',' ',Narrative_noTypos))
scripts <- scripts %>% 
  mutate(Narrative_noSpaces = gsub('cansometimes','can sometimes',Narrative_noSpaces)) %>%
  mutate(Narrative_noSpaces = gsub('ssaying','saying',Narrative_noSpaces))
scripts <- scripts %>% mutate(Narrative_Lemma = lemmatize_strings(Narrative_noSpaces))

                    

scripts <- scripts %>%
  select(Character1 = Character1_Lemma, Character2 = Character2_Lemma, Narrative = Narrative_Lemma) %>%
  filter(Character1 != "") %>%
  filter(Character2 != "")
  
scripts$ID <- seq.int(nrow(scripts))


# Frequency wordcloud
wordcloud(scripts$Narrative, scale=c(4,0.5), max.words=50, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))




