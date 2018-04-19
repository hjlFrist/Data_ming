library(arules)


#开始计算
a=read.transactions('C:/Users/Administrator/Desktop/wang.csv',sep = " ") # 导入数据,为稀疏矩阵，列为物品，行为每次的经历，数值为0,1是否包含
summary(a)
inspect(a[1:5])  # 查看前几行
itemFrequency(a[,1:3]) # 某个商品使用频数
itemFrequencyPlot(a,support=0.1,topN=20) # 显示数据中支持度至少为0.1的物品，排名前20.
image(a[1:5])  # 显示前5行所有物品
  #　image(sample(groceries,100))  随机抽取100行数据

myrules = apriori( data = a, parameter = list(support = 0.1, confidence = 0.9, minlen = 1))
#　myrules =cspade( data = a, parameter = list(support = 0.1, control = list(verbose = TRUE)))　
inspect(myrules[])
Summary(myrules)

inspect(sort(myrules,by = "support")[1:5]) # 选择最好的前5个规则 ，排序可以按照支持度，置信度，和lift(提升度)

setrules=subset(myrules, item %in% "good") # 查找包含good商品所有规则

write(myrules,file = "myrule.csv", seq=",")




