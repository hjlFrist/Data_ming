library("igraph")  

a=read.csv('C:/Users/Administrator/Desktop/df1.txt',sep = "|") # 读取数据，三列，第一列我开始节点，第二列为指向节点，最后一列为权重
g = graph.data.frame(a, directed = F) # T 为有向图，F为无向图

c=degree(g)
names(c)="degree"

d=data.frame(rownames(c),c$degree)


# GN算法
#karate  <-  graph.famous("Zachary")  
ebc <- edge.betweenness.community(g)  
modularity(ebc)  
membership(ebc)  
plot(ebc,g)



#Newman 算法
# karate  <-  graph.famous("Zachary")  
fc  <-  fastgreedy.community(g)  
fc <- cluster_fast_greedy(g)
dendPlot(fc)  
plot(fc,g)
plot(fc, g,vertex.size=5,vertex.label=NA)
b=data.frame(title=fc$names,member=fc$membership)
write.table(b,file="C:/Users/Administrator/Desktop/test.csv",row.names=FALSE,col.names=FALSE,sep=",")

# LPA算法
# karate  <-  graph.famous("Zachary")
community <- label.propagation.community(g)
modularity(community)
membership(community)
plot(community,g) 
plot(lc , g,vertex.size=5,vertex.label=NA)

# 聚类分析
# 边的中介度聚类
system.time(ec <- edge.betweenness.community(g))
print(modularity(ec))
plot(ec,g,vertex.size=5,vertex.label=NA)


#随机游走
system.time(wc <- walktrap.community(g))
print(modularity(wc))
#membership(wc)
plot(wc,g,vertex.size=5,vertex.label=NA)


#特征值（个人理解觉得类似谱聚类）

system.time(lec <-leading.eigenvector.community(g))
print(modularity(lec))
plot(lec,g,vertex.size=5,vertex.label=NA)


#多层次聚类

system.time(mc <- multilevel.community(g, weights=NA))
print(modularity(mc))
plot(mc, g,vertex.size=5,vertex.label=NA)


cluster_walktrap(g)
cluster_spinglass(g, spins=2) # spin 为群数
cluster_leading_eigen(g)



#文件输出

zz<-file("d:/test.txt","w")
cat(x,file=zz,sep="\n")
close(zz)
