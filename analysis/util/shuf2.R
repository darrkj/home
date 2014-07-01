library(boRg)
library(rpart)
data(orangeCar.train)

model <- function(data, shuff = TRUE, ...) {
  size <- nrow(data)
  # Number of obs in train set
  n <- ceiling(size * .75)
  y <- 1:size
  z <- sample(y)
  # Partition to train and test set
  train <- data[z[1:n],]
  if (shuff) {
    train$IsBadBuy <- 
      train$IsBadBuy[sample(length(train$IsBadBuy), ...)]
  }
  test <- data[z[(n+1):size],]

  rm(n, y, z)
  # Create decision tree model on training set
  rmod <- rpart(factor(IsBadBuy) ~ ., train,
                control = rpart.control(minsplit = 1,
                                        maxdepth = 6))

  # Evaluate model on test set
  pred <- predict(rmod, newdata = test)[,2]
  # Simple check of accuracy
  prd <- ifelse(pred > .5, 1, 0)
  acc <- sum(test$IsBadBuy == prd) / length(prd)
  return(acc)
}
its <- 500
###############################################################################


runModel <- function(data, ...) {
  Accuracy <- model(data, shuff = FALSE)
  for(i in seq(its)) {
    Accuracy <- c(Accuracy, model(data, ...))
    print(i)
  }
  return(Accuracy)
}

###############################################################################
# Run with all data.
cardata1 <- orangeCar.train
mean(cardata1$IsBadBuy)

Accuracy1 <- runModel(cardata1)

Accuracy1[1]
mean1 <- mean(Accuracy1[2:(its+1)])
sd1 <- sd(Accuracy1[2:(its+1)])
sd1l <- mean1 - 2 * sd1
sd1u <- mean1 + 2 * sd1

hist(Accuracy1, 40)
abline(v = Accuracy1[1], col = "red", lwd = 2)


###############################################################################

# Run with all rows but only color as input.
cardata2 <- orangeCar.train[,c("IsBadBuy", "Color")]
Accuracy2 <- runModel(cardata2)

Accuracy2[1]
mean2 <- mean(Accuracy2[2:(its+1)])
sd2 <- sd(Accuracy2[2:(its+1)])
sd2l <- mean2 - 2 * sd2
sd2u <- mean2 + 2 * sd2

hist(Accuracy2, 20)
abline(v = Accuracy2[1], col = "red", lwd = 2)

###############################################################################

# use replacement.
cardata3 <- orangeCar.train

Accuracy3 <- runModel(cardata3, replace = TRUE)

Accuracy3[1]
mean3 <- mean(Accuracy3[2:(its+1)])
sd3 <- sd(Accuracy3[2:(its+1)])
sd3l <- mean3 - 2 * sd3
sd3u <- mean3 + 2 * sd3

hist(Accuracy3, 20)
abline(v = Accuracy3[1], col = "red", lwd = 2)



###############################################################################

# Run with all rows but only color as input and use replacement.
cardata4 <- orangeCar.train[,c("IsBadBuy", "Color")]
Accuracy4 <- runModel(cardata4, replace = TRUE)

Accuracy4[1]
mean4 <- mean(Accuracy4[2:(its+1)])
sd4 <- sd(Accuracy4[2:(its+1)])
sd4l <- mean4 - 2 * sd4
sd4u <- mean4 + 2 * sd4

hist(Accuracy4, 20)
abline(v = Accuracy4[1], col = "red", lwd = 2)


###############################################################################


par(mfrow = c(2,2))

hist(Accuracy1, 40, main = "All Predictors")
abline(v = Accuracy1[1], col = "red", lwd = 2)
abline(v = mean1, col = "green", lwd = 2)
abline(v = sd1l, col = "green", lwd = 2, lty = 3)
abline(v = sd1u, col = "green", lwd = 2, lty = 3)

hist(Accuracy2, 20, main = "Only Color")
abline(v = Accuracy2[1], col = "red", lwd = 2)
abline(v = mean2, col = "green", lwd = 2)
abline(v = sd2l, col = "green", lwd = 2, lty = 3)
abline(v = sd2u, col = "green", lwd = 2, lty = 3)

hist(Accuracy3, 40, main = "All Predictors, With Replacement")
abline(v = Accuracy3[1], col = "red", lwd = 2)
abline(v = mean3, col = "green", lwd = 2)
abline(v = sd3l, col = "green", lwd = 2, lty = 3)
abline(v = sd3u, col = "green", lwd = 2, lty = 3)

hist(Accuracy4, 20, main = "Only Color, With Replacement")
abline(v = Accuracy4[1], col = "red", lwd = 2)
abline(v = mean4, col = "green", lwd = 2)
abline(v = sd4l, col = "green", lwd = 2, lty = 3)
abline(v = sd4u, col = "green", lwd = 2, lty = 3)



###############################################################################

# Reduce the size of zeros.
cardata3 <- orangeCar.train[with(orangeCar.train, order(-IsBadBuy)), ]
mean(cardata3$IsBadBuy)
cardata3 <- cardata3[1:50000,]
mean(cardata3$IsBadBuy)

Accuracy2 <- runModel(cardata3)
Accuracy3 <- model(cardata3)
for(i in seq(its)) {
  cardata3$IsBadBuy <- shuffle(cardata3$IsBadBuy)
  Accuracy3 <- c(Accuracy3, model(cardata3))
  print(i)
}

hist(Accuracy3, 20)
mean(Accuracy3[2:(its+1)])
sd(Accuracy3[2:(its+1)])

###############################################################################


# Reduce the size of zeros even more.
cardata4 <- orangeCar.train[with(orangeCar.train, order(-IsBadBuy)), ]
mean(cardata4$IsBadBuy)
cardata4 <- cardata4[1:30000,]
mean(cardata4$IsBadBuy)

Accuracy4 <- model(cardata4)
for(i in seq(its)) {
  cardata4$IsBadBuy <- shuffle(cardata4$IsBadBuy)
  Accuracy4 <- c(Accuracy4, model(cardata4))
  print(i)
}

hist(Accuracy4, 20)
mean(Accuracy4[2:(its+1)])
sd(Accuracy4[2:(its+1)])



###############################################################################
