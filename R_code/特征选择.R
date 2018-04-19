library(FSelector)

a=read.csv()

weights = random.forest.importance(Y1~.,train,importance.type = 1) 
subset=cutoff.k(weights,5)
f=as.simple.formula(subset,"class")

evaluator = function(subset){
  k=5
  set.seed(2)
  ind = sample(5,nrow(trainset),replace = TRUE)
  results= sapply(1:k, function(i){
    train = trainset[ind == i,]
    test = trainset[ind !=i,]
    tree = rpart(as.simple.formula(subset,"churn"),trainset)
    error.rate=sum(test$churn != predict(tree,test,type = "class"))/nrow(test)
    return(1-error.rate)
  })
  return(mean(results))
}

attr.subset = hill.climbing.search(names(trainset)[!names(trainset)%in%"churn"],evaluator)
f=as.simple.formula(attr.subset,"churn")
print(f)
