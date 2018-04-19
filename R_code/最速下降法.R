#  迭代计算学习矩阵
x_1<-read.csv("原始矩阵.csv",header=F)
x_1<-as.matrix(x_1)
b<-read.csv("用户兴趣矩阵3.1-3.29.csv")
a<-read.csv("全网矩阵2.23-3.29.csv")
a<-a[,-1]
a<-as.matrix(a)
a<-a^2 
b<-b[,-1]
b<-as.matrix(b)
r_1<-b-a*x_1; 
p_1<-t(a)*r_1;
q_1<-1/2*(p_1+t(p_1)); 
x_2<-x_1+(norm(r_1, "F")/norm(q_1,"F"))^2*q_1;
r_2<-b-a*x_2;
p_2<-t(a)*r_2;
q_2<-p_2-diag(t(p_2)*q_1)/(norm(q_1,"F"))^2*q_1; 
n<-100000  
for(k in 2:n){
  x_2<-x_2+(norm(r_2,"F")/norm(q_2,"F"))^2*q_2;
  r_2<-b-a*x_2;
  p_2<-t(a)*r_2;
  q_2<-p_2-diag(t(p_2)*q_2)/(norm(q_2,"F"))^2*q_2;
  k<-k+1;
}
R<-norm(r_2,"F") 
write.csv(x_2,"学习矩阵.csv")