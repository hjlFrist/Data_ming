library(stats)
library(fpc)

a=read.csv('C:/Users/Administrator/Desktop/input_k_means.csv',header = TRUE)

# ��������ͼ
#data=a[,2:33]
#dist.e=dist(data,method='euclidean')
#heatmap(as.matrix(dist.e),labRow = F, labCol = F)

a[is.na(a)]<-0  
K <- 2:8
round <- 30# ÿ�ε���30�Σ�����ֲ�����
rst <- sapply(K,function(i){
                   print(paste("K=",i))
                   mean(sapply(1:round,function(r){
                   print(paste("Round",r))
                   result <- kmeans(a[,2:33], i)
                   stats <- cluster.stats(dist(a[,2:33]), result$cluster)
                   stats$avg.silwidth
                    }))
})
plot(K,rst,type='l',main='����ϵ����K�Ĺ�ϵ', ylab='����ϵ��')
b<-data.frame(K=K,rst=rst)
for(i in 1:length(b$rst)){
  if(b[i,2] == max(rst)){
    t<- b[i,1]
  }
}
# ��γ�ȹ۲�
old.par <- par(mfrow = c(1,2))
clu <- kmeans(a[,2:33],t)
mds = cmdscale(dist(a[,2:33],method="euclidean"))
plot(mds, col=clu$cluster, main='kmeans����', pch = 19)
plot(mds, col=a$Species, main='ԭʼ����', pch = 19)
par(old.par)
clu$centers




