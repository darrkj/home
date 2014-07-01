library(boRg)
k <- gdfRange(gen1dData(100, 0.5), 
              treeModelTrainer, 1:5, obs=20)

j <- gdfRange(gen2dData(100, 0.5), 
              treeModelTrainer, 1:5, obs=20)

l <- gdfRange(gen3dData(100, 0.5), 
              treeModelTrainer, 1:10, obs=20)

plotGDF(k)

plotGDF(j)

plotGDF(l)


a <- 1:10000
f <- function (n) for (i in 1:n) b <- a^2
g <- function (n) for (i in 1:n) b <- a*a
system.time(f(10000))
system.time(f(5000))