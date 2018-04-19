library(mlbench)

set.seed(2)
p=mlbench.cassini(500)
plot(p$x)

ds=dbscan(dist(p$x),0.2,2,countmode = NULL, method = "dist")
plot(ds,p$x)