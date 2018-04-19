a<-read.csv("C:/Users/Administrator/Desktop/静态属性/offline.csv",header = F) ##标记移动和异网用户的线下稀疏矩阵
a1<-a[,-1]
b<-as.matrix(a1)
s<-svd(b)
for(i in 1:length(s$d)){
  if(sum(s$d[1:i])>=(sum(s$d)*0.9))break
}
m<-i
s$d[(m+1):length(s$d)]<-0
s1<-(s$u)%*%diag(s$d)%*%t(s$v) 
y<-data.frame()
n<-2000   ##要预测的异网用户数量
i<-1  
for (i in 1:n) {
  t<-2;
  j<-n+1;
  y[1,i]<-a[i,1]
  for(j in (n+1):nrow(s1)){
    e<-crossprod(s1[i,],s1[j,])/sqrt(crossprod(s1[i,],s1[i,])*crossprod(s1[j,],s1[j,])) ##上面的语句是：用余弦定理计算异网用户的相似移动用户
## e<-sqrt((s1[i,]-s1[j,])*(s1[i,]-s1[j,]))
##    if(is.nan(e)==F)    ##F表示FALSE
      if(e>0)  {y[t,i]<-a[j,1];t<-t+1}   ##e为相似度
    }
  }
write.csv(y,"C:/Users/Administrator/Desktop/静态属性/offline4.csv")   ##线下相似用户表
