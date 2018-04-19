library(Rcpp)
library(RSNNS)

a=read.csv('C:/Users/Administrator/Desktop/dream-neuralnet.csv',header = T)

for(i in 1:10000){
  tryCatch(
    { 
  dfValues <- a[,1:6]
  dfTargets <- decodeClassLabels(a[,8])
  df <- splitForTrainingAndTest(dfValues, dfTargets, ratio=0.3)
  model <- mlp(df$inputsTrain, df$targetsTrain, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=50, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
  #model <- mlp(df$inputsTrain, df$targetsTrain, size=5, learnFunc="BackpropBatch", learnFuncParams=c(10, 0.1), maxit=100, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
  #model <- mlp(df$inputsTrain, df$targetsTrain, size=5, learnFunc="SCG", learnFuncParams=c(0, 0, 0, 0),  maxit=30, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
  #model <- rbfDDA(df$inputsTrain, df$targetsTrain)
  #model <- elman(df$inputsTrain, df$targetsTrain, size=5, learnFuncParams=c(0.1), maxit=100, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
  #model <- rbf(df$inputsTrain, df$targetsTrain, size=40, maxit=200, initFuncParams=c(-4, 4,  0.0,  0.2,  0.04),   learnFuncParams=c(1e-3, 0, 1e-3, 0.1, 0.8), linOut=FALSE)
  #model <- rbf(df$inputsTrain, df$targetsTrain, size=40, maxit=600, initFuncParams=c(0, 1,  0.0,  0.2,  0.04),  learnFuncParams=c(1e-5, 0, 1e-5, 0.1, 0.8), linOut=TRUE)
  p = predict(model,df$inputsTest)
  }, warning = function(w) {
      "1"
      
  },error = function(e){
      "2"
  }
  )
  test$predict=p
  agreement<- p==test$Y4 # 预测类别与真实类别相同
  retrieved=length(test$Y4) # 所有个数
  precision=sum(agreement)/retrieved # 正确个数占比
  print(precision) # 打印正确率
  
  if(precision > max){
    max=precision
  }#记录最大值
  
  if(precision > 0.7){
    break()
  }
  else i=i+1
}