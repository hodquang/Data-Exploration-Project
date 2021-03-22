#install.packages("igraph")

library(igraph)


######  SNA ######
conversations <- scripts %>% group_by(Character1, Character2) %>% summarise(counts = n())

nodes <- c(as.character(conversations$Character1), as.character(conversations$Character2))
nodes <- unique(nodes)

### FULL MAP: Characters with less than 2 conversations
my_graph <- graph_from_data_frame(d=conversations, vertices=nodes, directed=FALSE)
my_graph <- delete_edges(my_graph, E(my_graph)[counts < 2])

plot(my_graph, 
     vertex.label.color = "Black",
     edge.color = "black",
     edge.arrow.size = 0.4,
     edge.arrow.width= 0.8,
     layout = layout_nicely(my_graph),
     edge.curved= 0.3)

# Delete irrelevant Characters
g <- delete_vertices(simplify(my_graph), degree(my_graph)==0)
plot(g, 
     vertex.label.color = "Black",
     edge.color = "black",
     edge.arrow.size = 0.4,
     edge.arrow.width= 0.8,
     layout = layout_nicely(g),
     edge.curved= 0.3)

# Characters with more than 15 conversations
my_graph <- graph_from_data_frame(d=conversations, vertices=nodes, directed=TRUE)
my_graph <- delete_edges(my_graph, E(my_graph)[counts < 15])
g <- delete_vertices(simplify(my_graph), degree(my_graph)==0)
g.b <- betweenness(g, directed = TRUE)
w1 <- E(my_graph)$counts
V(g)$label.cex = 1.3
plot(g, 
     vertex.label.color = "Red",
     #vertex.label.color = rep(brewer.pal(8, "Set2"),2000),
     edge.color = "black",
     vertex.size = sqrt(g.b)*2,
     edge.arrow.size = 0.3,
     edge.arrow.width= 0.8,
     edge.width = sqrt(w1)/2, 
     layout = layout_nicely(g),
     edge.curved= 0.3)

####### Analysis on Character Dobby ########

g184 <- make_ego_graph(g, 2, nodes = 'Tom riddle', mode = c("all"))[[1]]
g184

# Get a vector of geodesic distances of all vertices from vertex 'Hagrid' 
dists <- distances(g184, "Tom riddle")

# Create a color palette of length equal to the maximal geodesic distance plus one.
colors2 <- c("black", "blue", "orange", "red", "green")

# Set color attribute to vertices of network g184.
V(g184)$color <- colors2[dists+1]

# Visualize the network based on geodesic distance from vertex 184 (patient zero).
plot(g184, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color= rep(brewer.pal(8, "Set2"),2000),
     vertex.size = 7,
     edge.arrow.size = .03,
     edge.curved= .3)

total_conver = aggregate(conversations$counts, by= list(conversations$Character1),FUN=sum)

total_conver <- total_conver %>% filter(x>20)  

ggplot(total_conver, aes(x=reorder(Group.1, -x), y=x, fill=-x)) +
  geom_point() +
  #geom_bar() +
  geom_col() +
  ylab("Dialogue Count") +
  xlab("Characters") +
  labs(fill="Character1") +
  ggtitle("Dialogue Count in HP Movie 2") +
  coord_flip()

