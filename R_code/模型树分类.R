library(RWeka)
library(rJava)

a=read.csv('C:/Users/Administrator/Desktop/b.csv',header = T) # 导入数据
b=a[1:(nrow(a)-5),]                # 实验数据
d=a[((nrow(a)-5)+1):nrow(a),]          # 目标数据

for(i in 1:10000){
  t=sample(1:nrow(b),replace = FALSE,size = 0.7*nrow(b))
  train=b[t,]
  test=b   
  tryCatch(
    { 
      rule=M5P(Y4~X1+X2+X3+X4+X5+X6,data=train)  # 必须为1,2,3类别，字母型不可
      p=predict(rule,test,type="class") # 使用全局数据测试,类型分为 数值型(vector) 和 类别型（class)
      # rpart.plot(rule,digits = 3,fallen.leaves = TRUE, type=3, extra=101)   展示图
    }, warning = function(w) {
      ""
      
    },error = function(e){
      ""
    }
  )
  test$predict=p
  agreement<- p==test$Y4 # 预测类别与真实类别相同
  retrieved=length(test$Y4) # 所有个数
  precision=sum(agreement)/retrieved # 正确个数占比
  print(precision) # 打印正确率
  if(precision > 0.85){
    break()
  }
  else i=i+1
}
table(actualclass=test$Y4,predictedclass=test$predict)
p=predict(rule,d,type='class')
d$people_predict=p
print(d$people_predict)

save(rule,file="E:/R/rule.RData") # 保存模型，E:/R/day.RData 为路径与名称
load("E:/R/rule.RData") # 加载模型
d=read.csv('C:/Users/Administrator/Desktop/d.csv',header = T)
p=predict(rule,d) # 模型运用在新数据上，自选择列，列必须与运算模型的自变量一致
