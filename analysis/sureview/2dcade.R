# real data, last two are forced outliers
real <- data.frame(x1 = c(rnorm(98, 100, sd = 5), rnorm(98, 50, sd = 5), c(81), c(30), c(round(runif(2, 0, 120)))),
                   x2 = c(rnorm(98, 100, sd = 10), rnorm(98, 50, sd = 5), c(21), c(100), round(runif(2, 0, 120))))

plot(real)
# uniform data, red dots
uni <- data.frame(x1 = runif(nrow(real), min(real$x1), max(real$x1)),
                  x2 = runif(nrow(real), min(real$x2), max(real$x2)))

plot(uni)

# label brown dots
real$y <- 0
# lable red dots
uni$y <- 1

# add red dots and brown dots
data <- rbind(real, uni)
plot(data$x1, data$x2)

library(randomForest)

# build classifier
tree <- randomForest(as.factor(y) ~ ., data = data)

# the classifier probabilities
xx <- predict(tree, newdata = data, type = 'prob')[, 2]

# Removes uniform dist added values (red dots)
real$prob <- xx[1:nrow(real)]

real <- real[order(real$prob), ]
real <- real[order(real$prob, decreasing = TRUE), ]

#which(min(diff(real$prob)) == diff(real$prob))

n1 <- 4
n2 <- nrow(real) - n1


plot(real$x1, real$x2, col=c(rep("red", n1), rep("black", n2)))





head(rev(sort(real$prob)), 4)

# should contain 201 and 202
tail(order(rxx), 4)
