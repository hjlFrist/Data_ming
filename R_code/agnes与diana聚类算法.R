library(cluster)
file=read.csv()
agn.f=agnes(a,method = 'ward')
plot(agn.f,which.plot=1,cex=5,yaxt='n',main='agnes�����㷨ͼ')

dia.f=diana(a)
plot(dia.f,which.plot=2,main="diana�����㷨ͼ")

