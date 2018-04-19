X <- read.table("uspsdata-1.txt")
s <- read.table("uspscl-1.txt")
y <- s[,1]

X1 <- read.table("uspsdata.txt")
s1 <- read.table("uspscl.txt")
y1 <- s[,1]

adaboost <- function(X,y,b){
  alpha=c()
  allpars=c()
  for (i in 1:b){
    w = rep(1/nrow(X),nrow(X))
    par = train(X,w,y)
    par1 = par
    y_hat <- classify(X, par1)
    indicator <- get_indicator(y, y_hat, X)
    error = (w %*% indicator)/sum(w)
    alpha1 = log( (1-error) / error )
    #print(alpha1)
    #print(w)
    #print("_____")
    for (i in 1:nrow(X)) {
      w[i] <- w[i] * exp(alpha1 * indicator[i])
      
    }
    #print(w)
    allpars=rbind(allpars,par)
    alpha=rbind(alpha,alpha1)
  }
  return(list(allpars=allpars, alpha=alpha))
}

classify<-function(X,par){
  #print("Start")
  j = par[1]
  theta = par[2]
  m = par[3]  
  #cat( "theta , ",theta)
  #print(m)
  #print("0000")
  y_hat = rep(m, nrow(X))
  for(i in 1:nrow(X)){
    #cat("X[i,j] ", X[i,j], "theta " , theta ,"\n")
    if(X[i,j]<theta){
      y_hat[i] = -m
    }
  }
  return(y_hat)
}

get_indicator <- function (y, y_hat, X){
  indicator=rep(0,nrow(X))
  for(i in 1:nrow(X)){
    if (y_hat[i]!=y[i]){
      indicator[i]= 1
    }
  }
  return(indicator) 
}
train <- function(X, w, y){
  ret = c()
  j_str=0
  theta=0
  theta_temp = 0
  m=c(1,nrow(X))
  min = 10000000000
  for(j in 1:ncol(X)){
    for(i in 1:nrow(X)){
      #cat("current j, i: ", j, "+", i, "\n")
      theta_temp <- X[i,j]
      par<-c(j, theta_temp, m)
      y_hat <- classify(X, par)
      m = y_hat
      indicator <- get_indicator(y, y_hat, X)
      
      cost <- sum(w * indicator) / sum(w)
      #cat("Cost: ", cost, " theta: ", theta_temp,"\n")
      
      if(cost<min){
        min = cost
        j_str=j
        theta = theta_temp
      }      
    #cat(i ," + " , j, "---> official theta: ", theta_temp, "J: " , j_str, "cost: ", min, "\n")
    }
  }
  if (length(which(y == m)) < nrow(X)/2) {
    m <- -m
  }
  ret=c(j_str,theta, -m[j_str])
  return(ret)
}

agg_class<- function(X, alpha, allpars){
  #cat("nrow(X) " , nrow(X) , "\n")
  sumall = rep(0, nrow(X))
  #cat("length(sumall) " , length(sumall) , "\n")
  #print("--")
  #print(alpha)
  for(i in 1:nrow(allpars)){
    #print("sumall")
    par = allpars[i,]
    #print(ncol(alpha %*% classify(X, par)))
   # print(nrow(alpha %*% classify(X, par)))
   # print(length(sumall))
   # print("ALMOST")
    sumall = sumall + alpha %*% classify(X, par)
  }
  return(sign(sumall) )
}

eval_cross <- function(X,y,b){
  listres = c()
  err = c()
  split = sample(1:5, length(y), replace=T)
  for(i in 1:5){
    x_train = subset(X,split!=i)
    y_train = subset(y,split!=i)
    par = adaboost(x_train, y_train,b)
    listres = rbind(listres, par)
   #print(listres)
  }
  #test = listres[1,]
  #print("j?")
  #print(test)
  #print("----")
  #print(test$alpha)
  #print("test1")
  
  for (i in 1:5){
    x_test = subset(X,split==i)
    y_test = subset(y,split==i)
    par = listres[i,]
    alpha= par$alpha
    test = c()
    testa = alpha[1,]
    #print("---")
   #print(test)
   # print(length(test))
   # print("---")
    allpar= par$allpar
    #print(alpha[1,1])
    for(i in 1:length(testa)){
      allparS = c()
      alphaS = c()
      for(j in 1:i){
        allparS = rbind(allparS,allpar[j,])
        alphaS=rbind(alphaS, alpha[j,])
      }
      y_hat = agg_class(x_test, alphaS, allparS)
      t = sum( y_test != y_hat ) / length(y_test)
      print("T: ")
      print(t)
      t1= sum(get_indicator(y_test, y_hat, x_test)) /length(y_test)
      print("t1: ")
      print(t1)
      err = rbind(err, t )
    }
  }
  return(mean(err))
}


get_test_train_err = function(X,y,X1,y1,b){
  print(y)
  print("here")
  testerr = c()
  trainerr =c()
  for(i in 1:b){
    print("__________")
    print(i)
    print("start")
    trainerr[i] = eval_cross(X1,y1,i)
    testerr[i]=eval_cross(X,y,i)
    print(trainerr[i])
    print(testerr[i])

  }
  #plot(1:4,trainerr)
  print("train")
  print(trainerr)
  print("test")
  print(testerr)
  
  plot(trainerr,col = 2, xlab = "# iteration", ylab = "err")
  lines(trainerr, col = 2)
  #plot(testerr, col = 2, xlab = "# iteration", ylab = "err")
  #lines(testnerr, col = 2)
  
  
}
#atr = adaboost(X,y)

#c_hat=agg_class(X, atr$alpha, atr$allpars)
#print(c_hat)
#err = eval_cross(X,y)
#print(err)
get_test_train_err(X,y,X1,y1,11)
