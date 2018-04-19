library(class)
library(gmodels)

a=read.csv('C:/Users/Administrator/Desktop/b.csv',header = T) # 导入数据
b=a[1:(nrow(a)-5),]               # 实验数据
d=a[((nrow(a)-5)+1):nrow(a),]          # 目标数据

for(i in 1:20){
  t=sample(1:nrow(b),replace = FALSE,size = 0.7*nrow(b))
  train=b[t,c(1,2,3,4,5,6)]        # 自变量的选择
  test=b[,c(1,2,3,4,5,6)]
  train_label=b[t,7]               # 因变量的选择
  test_label=b[,7]
  tryCatch(
    { 
      test_pred=knn(train=train,test=test,cl=train_label,k=i)  
      # p=CrossTable(x=test_label,y=test_pred,prop.chisq=FALSE)# 目前无法循环迭代计算准确率，只有用交叉表观察
      retrieved=length(test_label)
      # 准确率=预测正确个数/总数
      precision=sum(test_label == test_pred)/retrieved
      print(precision)
      
    }, warning = function(w) {
      ""
      
    },error = function(e){
      ""
    }
  )

     if (precision>0.7){
       CrossTable(x=test_label,y=test_pred,prop.chisq=FALSE)
       break()
     }
     else  i=i+1
}



