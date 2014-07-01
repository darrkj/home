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
xx <- predict(tree, newdata = data, type = 'prob')[, 2]

# Removes uniform dist added values (red dots)
rxx <- xx[1:202]

head(rev(sort(rxx)), 2)

# should contain 201 and 202
tail(order(rxx), 4)

# In all cases 4 capture outliers, usually 3 does it, 2 works an awful lot


anom <- function(data)
  
  
  
  
  
  
  
  
  
  
  


# real data, last two are forced outliers
real <- data.frame(x1 = c(rnorm(99, 1000), rpois(99, 4)+50, c(41), c(200)),
                   x2 = c(rep('r', 99), rep('g', 98), rep('r', 3)))
# uniform data, red dots
uni <- data.frame(x1 = runif(nrow(real), min(real$x1), max(real$x1)),
                  x2 = sample(c('r', 'g'), nrow(real), replace = T))

# label brown dots
real$y <- 0
# lable red dots
uni$y <- 1

# add red dots and brown dots
data <- rbind(real, uni)

library(randomForest)

# build classifier
tree <- randomForest(as.factor(y) ~ ., data = data)

# the classifier probabilities
xx <- predict(tree, newdata = data, type = 'prob')[, 2]

# Removes uniform dist added values (red dots)
rxx <- xx[1:nrow(real)]

head(rev(sort(rxx)), 4)

# should contain 201 and 202
tail(order(rxx), 4)

