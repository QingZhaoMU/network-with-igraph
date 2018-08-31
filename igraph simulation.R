rm(list=ls())
library(igraph)

nl <- 50
ns <- 20
nc <- 5

seq1 <- sample(1:(nl-1), nc-1, replace=F)
seq1 <- c(0, sort(seq1), nl)
seq2 <- sample(1:(ns-1), nc-1, replace=F)
seq2 <- c(0, sort(seq2), ns)

# simulate a nl by ns incidence matrix 'a'
a <- matrix(0, nl, ns)
for (i in 1:nc) {
  a[(seq1[i]+1):seq1[i+1], (seq2[i]+1):seq2[i+1]] <- 1
}
table(rowSums(a))
table(colSums(a))

prob <- 0.01 # This probability governs the extent of hard clustering (0 for pure hard clustering)
more.one <- sample(c(0,1), size=length(which(a==0)), replace=T, prob=c(1-prob,prob))
a[which(a==0)] <- more.one

g <- graph_from_incidence_matrix(a, directed=FALSE, weighted=NULL)
is.bipartite(g)

me <- cluster_infomap(g, nb.trials=100)
md <- cluster_fast_greedy(g)
ceb <- cluster_edge_betweenness(g)

max(membership(me))
max(md$membership)
max(ceb$membership)

par(mfrow=c(2,3))
par(mar=c(.5,.5,0,0))
plot(g, vertex.color=c(rep('red',nl), rep('blue',ns)), vertex.label=c(1:nl, 1:ns))
plot(g, vertex.color=c(rep('red',nl), rep('blue',ns)), vertex.label=c(1:nl, 1:ns), layout=layout_as_bipartite)
plot(g, vertex.color=membership(me), vertex.label='')
plot(g, vertex.color=md$membership, vertex.label='')
dendPlot(ceb, mode="hclust")
plot(ceb, g)


