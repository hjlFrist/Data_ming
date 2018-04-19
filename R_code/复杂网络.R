library(igraph)
a<-read.csv('C:/Users/Administrator/Desktop/result_2.csv',header = T)
g = graph.data.frame(a, directed = F)
plot(g,layout = layout.fruchterman.reingold,
      vertex.size = 2,vertex.label.cex=0.7,vertex.color=V(g)$color,
      vertex.label.color=E(g)$color,edge.color = "blue",edge.arrow.size =0,
       edge.arrow.width =1,edge.lty=1,edge.label.cex=0.6,edge.curved=0,
        margin=-0.01,edge.loop.angle=1,vertex.label.dist=0.1)



tplot(g,vertex.size = 1,vertex.label.cex=0.7,vertex.color=V(g)$color,
     vertex.label.color=E(g)$color,edge.color = "blue",edge.arrow.size =0,
     edge.arrow.width =1,edge.lty=1,edge.label.cex=0.6,edge.curved=0,
     edge.loop.angle=1,vertex.label.dist=0.1)


## ×ÓÈº·Ö¸î

com = walktrap.community(gg, steps = 6)
sg = data.frame(name = com$labels, sg = com$mem + 1)
subgroup = vector("list", length(unique(com$mem)))
for(i in 1:length(unique(com$mem))){
  subgroup[[i]] = as.character(sg[sg[, 2]== i, 1])}
rm(i)
## subgroup
V(gg)$sg = com$mem + 1
V(gg)$color = rainbow(max(V(gg)$sg))[V(gg)$sg]
## png("net_walktrap.png", width = 500, height = 500)
par(mar = c(0, 0, 0, 0))
set.seed(14)
plot(gg, layout = layout.fruchterman.reingold, vertex.size = 5,
     vertex.color = V(gg)$color, vertex.label = NA, edge.color = grey(0.5),
     edge.arrow.mode = "-")
## dev.off()