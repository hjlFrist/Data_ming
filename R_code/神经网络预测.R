install.packages("neuralnet")
library(neuralnet)

a=read.csv('C:/Users/Administrator/Desktop/data.csv',header = TRUE)
b=a[,c(-1,-2)]
c=cor(b)

normalize = function(x){
  
  return((x-min(x))/(max(x)-min(x)))
  
}
concrete_norm=as.data.frame(lapply(b, normalize))

t=sample(1:nrow(concrete_norm),replace = FALSE,size = 0.7*nrow(concrete_norm))
train=concrete_norm[t,]
test=concrete_norm[-c(t),]
e=data.frame(matrix(NA,8,2))
# for (i in  1:8) {
m=neuralnet(people~AVG_PRS+MAX_PRS_Max+MIN_PRS_Min+MAX_WIN_S_Max+MAX_WIN_S_Inst_Max+avg_TEM+MAX_TEM_Max+MIN_TEM_Min+rijiaocha+avg_RHU+avg_VAP+min_RHU_Min+avg_PRE_1h+aqi+pm_25+pm_10+so_2+no_2+co+o_3,data = train,hidden=3)
# plot(m)
p=compute(m,test[2:21])
# ml_predict=predict(ml,newdata = test[,c(15,18,21)]) 
people_predict=p$net.result
e[i,1]=i
e[i,2]=cor(people_predict,test$people)
# }
plot(e$X1,e$X2,xlab="层数",ylab="相关�?",col=2)

lines(e$V1,e$V2,xlab="层数",ylab="相关�?",col=2)



# write.csv(t, file="C:/Users/Administrator/Desktop/t.csv")  
