library(igraph)
library(igraphdata)
install.packages("scales")                              # Install & load scales
library("scales")

#Year 1 plot
full_women_graph <- read.csv("edges50.txt", sep = ",")
women_graph <- graph_from_data_frame(full_women_graph, directed = T)

V(women_graph)$label.cex = 0.75
V(women_graph)$label.font = 2
plot(women_graph, vertex.size=10,vertex.color= "red",edge.arrow.size = 0.3,edge.color= "black", layout=layout_nicely(women_graph), edge.size=0.2, main="Year 1")

decomp = components(women_graph)
largest_comp = which.max(decomp$csize)
women = V(women_graph)[decomp$membership==largest_comp]
giant_comp = induced_subgraph(women_graph, women)
V(giant_comp)$label.font = 2
V(giant_comp)$label.cex= 0.75
plot(giant_comp, vertex.size=10,vertex.color= "red",edge.arrow.size = 0.3,edge.color= "black", layout=layout_nicely(giant_comp), edge.size=0.2, main="Year 1")

pos_max <- which.max(degree(giant_comp, mode="in"))
degree(giant_comp, mode="in")[pos_max]
pos_max <- which.max(degree(women_graph,mode="in"))
degree(women_graph, mode="in")[pos_max]

pos_max_2 <- which.max(closeness(giant_comp,mode="out"))
closeness(giant_comp,mode="out")[pos_max_2]
pos_max_2 <- which.max(closeness(women_graph,mode="out"))
closeness(women_graph, mode="out")[pos_max_2]

pos_max_3 <- which.max(betweenness(giant_comp,directed=TRUE, normalized=T))
betweenness(giant_comp, directed=TRUE, normalized = T)[pos_max_3]
pos_max_3 <- which.max(betweenness(women_graph, directed=TRUE, normalized=T))
betweenness(women_graph, directed=TRUE, normalized = T)[pos_max_3]

pos_max_4 <- which.max(eigen_centrality(giant_comp, directed=TRUE)$vector)
eigen_centrality(giant_comp, directed=TRUE)$vector[pos_max_4]
pos_max_4 <- which.max(eigen_centrality(women_graph, directed=TRUE)$vector)
eigen_centrality(women_graph, directed=TRUE)$vector[pos_max_4]

#Year 2 plot
full_women_graph2 <- read.csv("edges2_50.txt", sep = ",")
women_graph2 <- graph_from_data_frame(full_women_graph2, directed = T)

V(women_graph2)$label.cex = 0.75
V(women_graph2)$label.font = 2
plot(women_graph2, vertex.size=10,vertex.color= "red",edge.arrow.size = 0.3,edge.color= "black", layout=layout_nicely(women_graph2), edge.size=0.2, main="Year 2")

decomp = components(women_graph2)
largest_comp = which.max(decomp$csize)
women2 = V(women_graph2)[decomp$membership==largest_comp]
giant_comp = induced_subgraph(women_graph2, women2)
V(giant_comp)$label.font = 2
V(giant_comp)$label.cex= 0.75
plot(giant_comp, vertex.size=10,vertex.color= "red",edge.arrow.size = 0.3,edge.color= "black", layout=layout_nicely(giant_comp), edge.size=0.2, main="Year 2")

pos_max <- which.max(degree(giant_comp,mode="in"))
degree(giant_comp, mode="in")[pos_max]
pos_max <- which.max(degree(women_graph2,mode="in"))
degree(women_graph2, mode="in")[pos_max]
degree(women_graph2, mode="in")

pos_max_2 <- which.max(closeness(giant_comp,mode="out"))
closeness(giant_comp, mode="out")[pos_max_2]
pos_max_2 <- which.max(closeness(women_graph2,mode="out"))
closeness(women_graph2, mode="out")[pos_max_2]

pos_max_3 <- which.max(betweenness(giant_comp, directed=TRUE, normalized=T))
betweenness(giant_comp, directed=TRUE, normalized = T)[pos_max_3]
pos_max_3 <- which.max(betweenness(women_graph2, directed=TRUE, normalized=T))
betweenness(women_graph2, directed=TRUE, normalized = T)[pos_max_3]

pos_max_4 <- which.max(eigen_centrality(giant_comp, directed=TRUE)$vector)
eigen_centrality(giant_comp, directed=TRUE)$vector[pos_max_4]
pos_max_4 <- which.max(eigen_centrality(women_graph2, directed=TRUE)$vector)
eigen_centrality(women_graph2, directed=TRUE)$vector[pos_max_4]

#Year 3
full_women_graph3 <- read.csv("edges3_50.txt", sep = ",")
women_graph3 <- graph_from_data_frame(full_women_graph3, directed = T)

V(women_graph3)$label.cex = 0.75
V(women_graph3)$label.font = 2
plot(women_graph3, vertex.size=10,vertex.color= "red",edge.arrow.size = 0.3,edge.color= "black", layout=layout_nicely(women_graph3), edge.size=0.2, main="Year 3")

decomp = components(women_graph3)
largest_comp = which.max(decomp$csize)
women3 = V(women_graph3)[decomp$membership==largest_comp]
giant_comp = induced_subgraph(women_graph3, women3)
V(giant_comp)$label.font = 2
V(giant_comp)$label.cex= 0.75
plot(giant_comp, vertex.size=10,vertex.color= "red",edge.arrow.size = 0.3,edge.color= "black", layout=layout_nicely(giant_comp), edge.size=0.2, main="Year 3")

pos_max <- which.max(degree(giant_comp,mode="in"))
degree(giant_comp, mode="in")[pos_max]
pos_max <- which.max(degree(women_graph3,mode="in"))
degree(women_graph3, mode="in")[pos_max]

pos_max_2 <- which.max(closeness(giant_comp, mode="out"))
closeness(giant_comp, mode="out")[pos_max_2]
pos_max_2 <- which.max(closeness(women_graph3,mode="out"))
closeness(women_graph3, mode="out")[pos_max_2]

pos_max_3 <- which.max(betweenness(giant_comp, directed=TRUE, normalized=T))
betweenness(giant_comp, directed=TRUE, normalized = T)[pos_max_3]
pos_max_3 <- which.max(betweenness(women_graph3, directed=TRUE, normalized=T))
betweenness(women_graph3, directed=TRUE, normalized = T)[pos_max_3]

pos_max_4 <- which.max(eigen_centrality(giant_comp, directed=TRUE)$vector)
eigen_centrality(giant_comp, directed=TRUE)$vector[pos_max_4]
pos_max_4 <- which.max(eigen_centrality(women_graph3, directed=TRUE)$vector)
eigen_centrality(women_graph3, directed=TRUE)$vector[pos_max_4]

#Alc Year 1
nodess <- read.csv("alc_nodes.txt", sep = ",")
full_women_graph
alc <- graph_from_data_frame(full_women_graph, vertices=nodess, directed = TRUE)
alc2 <- induced_subgraph(alc, which(V(alc)$alc==1))
V(alc2)$label.cex = 0.75
V(alc2)$label.font = 2
plot(alc2, vertex.size=10, vertex.color= "light blue", edge.arrow.size = 0.2,edge.color= "black", edge.size=0.2, main="Year 1 Alcohol No Drinking")

alc3 <- induced_subgraph(alc, which(V(alc)$alc==2))
V(alc3)$label.cex = 0.75
V(alc3)$label.font = 2
plot(alc3, vertex.size=10, vertex.color= "lavender", edge.arrow.size = 0.2,edge.color= "black", edge.size=0.2, main="Year 1 Alcohol Drinking 1/2x A Year")

alc4 <- induced_subgraph(alc, which(V(alc)$alc==3))
V(alc4)$label.cex = 0.75
V(alc4)$label.font = 2
plot(alc4, vertex.size=10, vertex.color= "palegreen3", edge.arrow.size = 0.2,edge.color= "black", edge.size=0.2, main="Year 1 Alcohol 1x A Month")

alc5 <- induced_subgraph(alc, which(V(alc)$alc==4))
V(alc5)$label.cex = 0.75
V(alc5)$label.font = 2
plot(alc5, vertex.size=10, vertex.color= "lightgoldenrodyellow", edge.arrow.size = 0.2,edge.color= "black", edge.size=0.2, main="Year 1 Alcohol 1x A Week")

alc6 <- induced_subgraph(alc, which(V(alc)$alc==5))
V(alc6)$label.cex = 0.75
V(alc6)$label.font = 2
plot(alc6, vertex.size=10, vertex.color= "light coral", edge.arrow.size = 0.2,edge.color= "black", edge.size=0.2, main="Year 1 Alcohol >1x A Week")
#layout from stack overflow https://stackoverflow.com/questions/37378744/igraph-grouped-layout-based-on-attribute
G_grouped = alc
E(G_grouped)$weight = 1
for(i in unique(V(alc)$alcc)) {
  GroupV = which(V(alc)$alcc == i)
  G_grouped = add_edges(G_grouped, combn(GroupV, 2), attr=list(weight=10))
}
set.seed(567)
Layout1 = layout_with_fr(G_grouped)
V(alc)$label.cex = 0.75
V(alc)$label.font = 2
kc = cluster_edge_betweenness(alc, directed=TRUE, modularity=TRUE)
length(kc)
sizes(kc)
modularity(kc)

plot(kc, alc, vertex.size=10, col= V(alc)$alcc, edge.arrow.size = 0.1,edge.color= "black", edge.size=0.2, main="Year 1 Alcohol")
plot(alc, vertex.size=10, layout=Layout1, col= V(alc)$alcc, edge.arrow.size = 0.1,edge.color= "black", edge.size=0.2, main="Year 1 Alcohol")
legend(x= "topleft", legend=c("1 (non)", "2 (once or twice a year)", "3 (once a month)", "4 (once a week)", "5 (more than once a week)"), 
       pt.bg  = c("blue", "purple", "green", "yellow", "red"), pt.cex =1.5, pch    = 21, cex    = 0.5, bty    = "n")

#Avgs
pos = which(V(alc)$alc==1)
mean(degree(alc, mode="in")[pos])
mean(closeness(alc,normalized=T)[pos])
mean(betweenness(alc,normalized=T)[pos])
mean(eigen_centrality(alc)$vector[pos])

pos2 = which(V(alc)$alc==2)
mean(degree(alc, mode="in")[pos2])
mean(closeness(alc,normalized=T)[pos2])
mean(betweenness(alc,normalized=T)[pos2])
mean(eigen_centrality(alc)$vector[pos2])

pos3 = which(V(alc)$alc==3)
mean(degree(alc, mode="in")[pos3])
mean(closeness(alc,normalized=T)[pos3])
mean(betweenness(alc,normalized=T)[pos3])
mean(eigen_centrality(alc)$vector[pos3])

pos4 = which(V(alc)$alc==4)
mean(degree(alc, mode="in")[pos4])
mean(closeness(alc,normalized=T)[pos4])
mean(betweenness(alc,normalized=T)[pos4])
mean(eigen_centrality(alc)$vector[pos4])

pos5 = which(V(alc)$alc==5)
mean(degree(alc, mode="in")[pos5])
mean(closeness(alc,normalized=T)[pos5])
mean(betweenness(alc,normalized=T)[pos5])
mean(eigen_centrality(alc)$vector[pos5])

degree(alc)

y= degree(alc)
x=V(alc)$alc
plot(x, y,lwd =5, xlab="ALcohol Usage", ylab = "Degree")
abline(lm(y ~ x), col = "blue")

