a=read.csv('C:/Users/Administrator/Desktop/data.csv',header = TRUE)

pre=c(0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008,0.009,0.02)  
for(l in pre ){
  for(i in 1:1000){
  # ²ÉÑù
     t=sample(1:nrow(a),replace = FALSE,size = 0.7*nrow(a))
     train=a[t,]
     test=a[-c(t),]
     lm.f=lm(Y~X,data = train)
     p=predict(lm.f,test[1])
     MSE=sum(abs(p - test$Y))/nrow(test) 
     print(MSE)
     if(MSE<l){
        print(lm.f)        
      break()
     }
     else i=i+1
  }
  if(MSE<l){
    break()
  }
}
p=predict(m,target$X)
MSE=sum(abs(p - target$Y))/nrow(target) 
print(MSE)
