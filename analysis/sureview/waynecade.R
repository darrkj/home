library (class)

dataset <- iris[,1:4]
colnames (dataset) <- c("x1", "x2", "x3", "x4")

n.good <- nrow (dataset)
n.bad  <- 15
n.rand <-150

# "Bad" data points, in a range selected by eye...
bad <- data.frame (x1=runif (n.bad, 6.0, 8.5), x2=runif (n.bad, 3.5, 5.0), x3=runif (n.bad, 2.5, 7.5), x4=runif (n.bad, -0.5, 1.5))

dataset2 <- rbind (dataset, bad)

plot (dataset2, col=c(rep ("black", n.good), rep ("red", n.bad)))

dmin <- apply (dataset2, 2, min) - 0.5
dmax <- apply (dataset2, 2, max) + 0.5

rand <- data.frame (x1=runif (n.rand, dmin[1], dmax[1]), x2=runif (n.rand, dmin[2], dmax[2]), x3=runif (n.rand, dmin[3], dmax[3]), x4=runif (n.rand, dmin[4], dmax[4]))
traintest <- rbind (dataset2, rand)

cl <- c(rep ("Good", n.good + n.bad), rep ("Bad", n.rand))

res <- knn (traintest, traintest, cl, k=10, prob=T)
print (res)

cat (sprintf ("There are %d bad items, with the top-ranked being:\n", n.bad))
print (order (order (round (ifelse (res == "Good", attr (res, "prob"), 1 - attr (res, "prob")), 2)[1:(n.good+n.bad)]))[(n.good+1):(n.good+n.bad)])
