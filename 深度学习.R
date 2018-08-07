library(h2o)
h2o.init()

a=read.csv('C:/Users/Administrator/Desktop/关联分析/3.csv',header = TRUE)
h=a

t=sample(1:nrow(a),replace = FALSE,size = 0.7*nrow(a))
train=a[t,]
test=a[-c(t),]    # 训练与测试集合

train.hex <- as.h2o(train) # 变成特定格式
test.hex <- as.h2o(test)
h.hex <- as.h2o(h)

dl <- h2o.deeplearning(x=2:11,y=1,training_frame = train.hex,activation = "Tanh",balance_classes = TRUE,hidden = c(100, 100, 100),epochs = 100)
# 预测的指标列，# 预测的类别列

model # 结果

the_predict <- h2o.predict(model, h.hex)
as.data.frame(the_predict)
h$predict=the_predict

