library(igraph)

a<-read.csv('data0208.csv',header = T)
g = graph.data.frame(a, directed = F)
edge_attr(g, "label")<-E(g)$"系数"
xs<-E(g)$"系数"
#g = simplify(g)
#E(g)$cex=0.6
#E(g)[c("网红","微博")]$cex=1
#E(g)$color="blue"
#E(g)[c("网红","微博")]$color="red"
V(g)$color="green"
#V(g)[c("王宝强事件")]$color="yellow"
#V(g)[c("英国")]$color="red"
#V(g)[c("脱欧")]$color="red"
#V(g)[c("同花顺")]$color="red"
#V(g)[c("结果")]$color="red"
#V(g)[c("公布")]$color="red"
#V(g)[c("表示")]$color="red"
plot(g,layout = layout.fruchterman.reingold,vertex.size = 8,vertex.label.cex=0.7
     ,vertex.color=V(g)$color,vertex.label.color=E(g)$color,edge.color = "blue",
     edge.arrow.size =0,edge.arrow.width =1,edge.lty=1,edge.label.cex=0.6,
     edge.curved=0,margin=-0.1,edge.loop.angle=1,vertex.label.dist=0.1)
