
# 设计程序循环计算N次，如果有一个满意结果，输出结果
a=read.csv('C:/Users/Administrator/Desktop/data.csv',header = TRUE)
b=a[1:1368,]
c=a[1359:1569,]
# 循环计算
for(i in 1:1000){
# 采样
     t=sample(1:nrow(b),replace = FALSE,size = 0.75*nrow(b))
     train=b[t,]
     test=b[-c(t),]
# 建模
     glm.fit=glm(Y~X,data=train,family=binomial(link="logit"))
# 模型验证,转化为0,1结果
     p=predict(glm.fit,test[1])
     p=exp(p)/(1+exp(p))
# 预测值
    test$T1_predicted=1*(p>0.5)
    true_value=test$Y
    predict_value=test$T1_predicted
    retrieved=length(predict_value)
# 准确率=预测正确个数/总数
    precision=sum(true_value == predict_value)/retrieved
    print(precision)
 # 输出准确率大于一定值的模型
    if (precision>0.65){
         print(glm.fit)
         break()
         }
     else  i=i+1
}
p=predict(glm.fit,c[1])
p=exp(p)/(1+exp(p))
# 预测值
c$y_predicted=1*(p>0.5)
true_value=c$Y
predict_value=c$y_predicted
retrieved=length(predict_value)
# 准确率=预测正确个数/总数
precision=sum(true_value == predict_value)/retrieved
print(precision)
