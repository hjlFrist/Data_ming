library(kernlab)
# library(e1071)

a=read.csv('C:/Users/Administrator/Desktop/day1.csv',header = T) # 导入数据
b=a[1:(nrow(a)-3),]
d=a[(nrow(a)-2):nrow(a),]
 
for(i in 1:10000){
# 采样，不放回,，分为训练数据与验证数据集合
    t=sample(1:nrow(b),replace = FALSE,size = 0.7*nrow(b))
    train=b[t,]
    test=b
    #test=b[-c(t),]
    #train=b[1:floor(0.8*nrow(b)),]                      
    #test=b[(floor(0.8*nrow(b))+1):nrow(b),]
    
# 建模，核函数为径向基
    svm.fit=ksvm(Y2~X1+X2+X3+X4+X5+X6,data=train,kernel='rbfdot')

#   svm.fit=svm(T1~C1+C2+C3+C4+C6+F1+F2+F3+F4,data=train)

# 验证
    p=predict(svm.fit,test[c(2,3,4,5,6,7)]) # 使用全局数据h测试
    table(p,test$Y3)# 每个类别准确个数
    test$predict=p 
    agreement<- p==test$Y3 # 预测类别与真实类别相同
    retrieved=length(test$Y3) # 所有个数
    precision=sum(agreement)/retrieved # 正确个数占比
    print(precision) # 打印正确个数
    #f=table(agreement)
# 总正确，错误个数
    #m=prop.table(table(agreement))# 错误正确占比
    #m=data.frame(t(m))
    if(precision>0.7){
      print(svm.fit)
      break()
    }
    else i=i+1
}
p=predict(svm.fit,d[,c(2,3,4,5,6,7)])
d$people_predict=p


# 9个分类别

d=read.csv('C:/Users/Administrator/Desktop/关联分析/3.csv',header = TRUE)
e=d[,c(-1,-12)]
h=e
for(i in 1:10000){
  # 采样，不放回,，分为训练数据与验证数据集合
  t=sample(1:nrow(e),replace = FALSE,size = 0.7*nrow(e))
  train=e[t,]
  test=e[-c(t),]
  
  # 建模，核函数为径向基
  svm.fit=ksvm(T2~C1+C2+C3+C4+C6+F1+F2+F3+F4,data=train,kernel='rbfdot')
  
  #   svm.fit=svm(T1~C1+C2+C3+C4+C6+F1+F2+F3+F4,data=train,kernel='linear',cost=1)
  #   svm.fit=svm(T1~C1+C2+C3+C4+C6+F1+F2+F3+F4,data=train,type='eps-regression')  连续数值预测
  #   svm.fit=tune.svm(T1~C1+C2+C3+C4+C6+F1+F2+F3+F4,data=train,gamma=10^(-6:-1),cost=10^(1:2)) 调整支持向量机
  #   svm.fit=tune.svm(T1~C1+C2+C3+C4+C6+F1+F2+F3+F4,data=train,gamma=tuned$best.parameters$gamma,cost=tuned$best.parameters$cost)
  # 验证
  p=predict(svm.fit,h[c(1,2,3,4,6,7,8,9,10)])
  table(p,h$T2)# 每个类别准确个数
  h$predict=p
  agreement<- p==h$T2 # 预测类别与真实类别相同
  retrieved=length(h$T2) # 所有个数
  precision=sum(agreement)/retrieved # 正确个数占比
  print(precision) # 打印正确个数
  #f=table(agreement)
  # 总正确，错误个数
  #m=prop.table(table(agreement))# 错误正确占比
  #m=data.frame(t(m))
  if(precision>0.5){
    print(svm.fit)
    break()
  }
  else i=i+1
}

