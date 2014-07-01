

uni <- function(x, len = length(x)) {
  # stopifnot(is.vector(x)) # Does not work for factors, not atomic type.
  if ( is.integer(x) ) {
    sample(min(x):max(x), len, replace = TRUE)
  } else if ( is.numeric(x) ) {
    runif(len, min(x), max(x))
  } else if ( is.factor(x) ) {
    factor(sample(levels(x), len, replace = TRUE))
  } else {
    sample(unique(x), len, replace = TRUE)
  }
}


# TODO: Add method to use different classifier
# TODO: Should bounds on uniform be scaled past min/max range
# prop allows you to throttle the 50/50 split, higher means more fake data.

cade1d <- function(df) {
  real <- data.frame(x1 = df)
  # create similar but uniform data
  fake <- data.frame(x1 = uni(df))
  
  # label brown dots
  real$y <- 0
  # lable red dots
  fake$y <- 1
  
  # add red dots and brown dots
  data <- rbind(real, fake)
  
  library(randomForest)
  # build classifier
  tree <- randomForest(as.factor(y) ~ ., data = data)
  
  # the classifier probabilities
  prob <- predict(tree, newdata = data, type = 'prob')[1:nrow(real), 2]
  prob / (1 - prob)
}





# real data, last two are forced outliers
real <- data.frame(x1 = c(rnorm(98, 100, sd = 5), rnorm(98, 50, sd = 5), c(78), c(30)),
                   x2 = c(rnorm(98, 100, sd = 10), rnorm(98, 50, sd = 5), c(21), c(100)))

plot(real)


real$prob1 <- cade1d(real$x1)
real$prob2 <- cade1d(real$x2)

