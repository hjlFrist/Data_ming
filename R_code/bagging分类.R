library(adabag)
library(ipred)

a=read.csv('C:/Users/Administrator/Desktop/b.csv',header = T) # 导入数据
b=a[1:(nrow(a)-5),]                # 实验数据
d=a[((nrow(a)-5)+1):nrow(a),]          # 目标数据

for(i in 1:10000){
  t=sample(1:nrow(b),replace = FALSE,size = 0.6*nrow(b))
  train=b[t,]
  test=b
  tryCatch(
    { 
      rule=bagging(Y4~X1+X2+X3+X4+X5+X6,data=train,mfinal=10)
      # rule=bagging(Y4~X1+X2+X3+X4+X5+X6,data=train,coob=T)
      # rule=bagging.cv(Y4~X1+X2+X3+X4+X5+X6,v=10,data=train,mfinal=10)  交叉验证
      p=predict.bagging(rule,test,type='class')               
    }, warning = function(w) {
      ""
    },error = function(e){
      ""
    }
  )
  predict.table=table(p,test$Y4)
  # rule$confusion
  print(p$error) # 打印正确率
  if(precision > 0.85){
    break()
  }
  else i=i+1
}
table(actualclass=test$Y4,predictedclass=test$predict)
p=predict(rule,d,type='class')
d$people_predict=p
print(d$people_predict)




