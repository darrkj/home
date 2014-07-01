# real data, last two are forced outliers
real <- c(rnorm(100, 1000), rpois(100, 4)+50, c(41), c(200))
# uniform data, red dots
uni <- runif(length(real), min(real), max(real))

# label brown dots
one <- data.frame(x1 = real, y = 0)
# lable red dots
two <- data.frame(x1 = uni, y = 1)

# add red dots and brown dots
data <- rbind(one, two)

library(randomForest)

# build classifier
tree <- randomForest(as.factor(y) ~ x1, data = data)

# the classifier probabilities
data$pred <- predict(tree, newdata = data, type = 'prob')[, 2]

# Removes uniform dist added values (red dots)
data <- data[data$y!=1,]

data <- data [order(-data[,3]),] #sort descending by predictions

head(data, 2)

# should contain 201 and 202

# In all cases 4 capture outliers, usually 3 does it, 2 works an awful lot
