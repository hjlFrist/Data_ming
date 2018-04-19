library(mclust)

my_data=read.csv()
m_clust <- Mclust(as.matrix(my_data), G=1:20)
summary(m_clust)
plot(m_clust, "BIC")


library(NbClust)
nb_clust <- NbClust(dataset,  distance = "euclidean",
                    min.nc=2, max.nc=15, method = "kmeans",
                    index = "alllong", alphaBeale = 0.1)
barplot(table(nb_clust$Best.nc[1,]),xlab = "聚类数",ylab = "支持指标数")



library(factoextra)
library(ggplot2)
set.seed(1234)
fviz_nbclust(dataset, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
km.res <- kmeans(dataset,3)
fviz_cluster(km.res, data = dataset)

library(fpc)
library(cluster)
pamk.best <- pamk(dataset)
pamk.best$nc
clusplot(pam(dataset, pamk.best$nc))

library(vegan)
ca_clust <- cascadeKM(dataset, 1, 10, iter = 1000)
ca_clust$results
calinski.best <- as.numeric(which.max(ca_clust$results[2,]))
calinski.best
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski<-as.data.frame(ca_clust$results[2,])
calinski$cluster <- c(1:10)
library(ggplot2)
ggplot(calinski,aes(x = calinski[,2], y = calinski[,1]))+geom_line()



library(apcluster)
ap_clust <- apcluster(negDistMat(r=2), dataset)
length(ap_clust@clusters)
heatmap(ap_clust)


require(cluster)
library(factoextra)
fviz_nbclust(dataset, kmeans, method = "silhouette")













