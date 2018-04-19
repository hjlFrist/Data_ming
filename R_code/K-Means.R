library(stats)
library(fpc)

a=read.csv('C:/Users/Administrator/Desktop/input_k_means.csv',header = TRUE)

# 数据热力图
#data=a[,2:33]
#dist.e=dist(data,method='euclidean')
#heatmap(as.matrix(dist.e),labRow = F, labCol = F)

a[is.na(a)]<-0  
K <- 2:8
round <- 30# 每次迭代30次，避免局部最优
rst <- sapply(K,function(i){
                   print(paste("K=",i))
                   mean(sapply(1:round,function(r){
                   print(paste("Round",r))
                   result <- kmeans(a[,2:33], i)
                   stats <- cluster.stats(dist(a[,2:33]), result$cluster)
                   stats$avg.silwidth
                    }))
})
plot(K,rst,type='l',main='轮廓系数与K的关系', ylab='轮廓系数')
b<-data.frame(K=K,rst=rst)
for(i in 1:length(b$rst)){
  if(b[i,2] == max(rst)){
    t<- b[i,1]
  }
}
# 降纬度观察
old.par <- par(mfrow = c(1,2))
clu <- kmeans(a[,2:33],t)
mds = cmdscale(dist(a[,2:33],method="euclidean"))
plot(mds, col=clu$cluster, main='kmeans聚类', pch = 19)
plot(mds, col=a$Species, main='原始聚类', pch = 19)
par(old.par)
clu$centers





