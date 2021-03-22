library(igraph)
library(RColorBrewer)

##### Sentence Sentiment

# Data source - Opinion Lexicon
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
#######################

pos.words <- scan("5.1 pve.csv",what = 'character')
neg.words <- scan("5.2 nve.csv",what = 'character')
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
  
  scores.df = data.frame(score=scores)
  return(scores)
}

scripts$result <- score.sentiment(scripts$Narrative,pos.words,neg.words)

total_score_char1=aggregate(scripts$result, by= list(scripts$Character1),FUN=sum)
total_score_char2=aggregate(scripts$result, by= list(scripts$Character2),FUN=sum)

#write.csv(total_score_char1, file = "total_score_char1.csv",row.names=FALSE, na="")
#write.csv(total_score_char2, file = "total_score_char2.csv",row.names=FALSE, na="")
#write.csv(nodes, file = "nodes.csv",row.names=FALSE, na="")
# manually edit the nodes file to add in the sentiment score
scripts$sentiment=ifelse(scripts$result==0,"Neutral",ifelse(scripts$result>0,"Positive","Negative"))

# new nodes
new_nodes <- read.csv("5.6 new_nodes.csv")
new_nodes$x <- unique(nodes)
`%notin%` <- Negate(`%in%`)
new_nodes <- new_nodes %>% filter(x %notin% c("Everyone"))
conversations <- conversations %>% filter(Character1 %notin% c("Everyone")) %>% filter(Character2 %notin% c("Everyone"))

my_graph <- graph_from_data_frame(d=conversations, vertices=new_nodes, directed=TRUE)
my_graph <- delete_edges(my_graph, E(my_graph)[counts < 5])
g <- delete_vertices(simplify(my_graph), degree(my_graph)==0)
g.b <- betweenness(g, directed = TRUE)
w1 <- E(my_graph)$counts
V(g)$label.cex = 1.3

V(g)$color <- ifelse(V(g)$score2 == 0, "Orange", ifelse(V(g)$score2 > 0, "blue", "Red"))
plot(g, 
     vertex.label.color = "Black",
     #vertex.label.color = rep(brewer.pal(8, "Set2"),2000),
     edge.color = rep(brewer.pal(8, "Set2"),2000),
     vertex.size = 10,
     edge.arrow.size = 0.3,
     edge.arrow.width= 0.8,
     edge.width = sqrt(w1)/2, 
     layout = layout_nicely(g),
     edge.curved= 0.3)


