library(psych)
library(GPArotation)


a=read.csv()
f.pca=prcomp(a,center = TRUE,scale = TRUE )
# f.pca=princomp(a,center = TRUE,scale = TRUE )
# f.pca=principal(a,nfactors = 5, rotate = "none" )
summary(f.pca)
