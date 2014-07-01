library(boRg)
data(orangeCar.train)
data(orangeCar.test)


model <- function(data) {
  size <- nrow(data)
  # Number of obs in train set
  n <- ceiling(size * .75)
  y <- 1:size
  z <- sample(y)
  # Partition to train and test set
  train <- data[z[1:n],]
  test <- data[z[(n+1):size],]

  rm(n, y, z)
  # Create decision tree model on training set
  rmod <- rpart(IsBadBuy ~ ., data,
                control = rpart.control(minsplit = 1, 
                                        cp = 0.0007,
                                        maxdepth = 8))

  # Evaluate model on test set
  pred <- predict(rmod, newdata = test)
  # Simple check of accuracy
  prd <- ifelse(pred > .5, 1, 0)
  acc <- sum(test$IsBadBuy == prd) / length(prd)
  return(acc)
}

###############################################################################

# Run with all data.
mean(orangeCar.train$IsBadBuy)
cardata1 <- orangeCar.train
Accuracy1 <- model(cardata1)

for(i in seq(100)) {
  cardata1$IsBadBuy <- shuffle(cardata1$IsBadBuy)
  Accuracy1 <- c(Accuracy1, model(cardata1))
  print(i)
}

hist(Accuracy1, 30)
mean(Accuracy1[2:101])
sd(Accuracy1[2:101])

###############################################################################

# Run with all rows but only color as input.
mean(orangeCar.train$IsBadBuy)
cardata2 <- orangeCar.train[,c("IsBadBuy", "Color")]
Accuracy2 <- model(cardata2)

for(i in seq(100)) {
  cardata2$IsBadBuy <- shuffle(cardata2$IsBadBuy)
  Accuracy2 <- c(Accuracy2, model(cardata2))
  print(i)
}

hist(Accuracy2, 20)
mean(Accuracy2[2:101])
sd(Accuracy2[2:101])

###############################################################################

# Reduce the size of zeros.
cardata3 <- orangeCar.train[with(orangeCar.train, order(-IsBadBuy)), ]
mean(cardata3$IsBadBuy)
cardata3 <- cardata3[1:50000,]
mean(cardata3$IsBadBuy)

Accuracy3 <- model(cardata3)
for(i in seq(100)) {
  cardata3$IsBadBuy <- shuffle(cardata3$IsBadBuy)
  Accuracy3 <- c(Accuracy3, model(cardata3))
  print(i)
}

hist(Accuracy3, 20)
mean(Accuracy3[2:101])
sd(Accuracy3[2:101])

###############################################################################


# Reduce the size of zeros even more.
cardata4 <- orangeCar.train[with(orangeCar.train, order(-IsBadBuy)), ]
mean(cardata4$IsBadBuy)
cardata4 <- cardata4[1:30000,]
mean(cardata4$IsBadBuy)

Accuracy4 <- model(cardata4)
for(i in seq(100)) {
  cardata4$IsBadBuy <- shuffle(cardata4$IsBadBuy)
  Accuracy4 <- c(Accuracy4, model(cardata4))
  print(i)
}

hist(Accuracy4, 20)
mean(Accuracy4[2:101])
sd(Accuracy4[2:101])



###############################################################################


# use replacement.
cardata5 <- orangeCar.train

Accuracy5 <- model(cardata5)
for(i in seq(100)) {
  cardata5$IsBadBuy <- shuffle(cardata5$IsBadBuy)
  Accuracy5 <- c(Accuracy5, model(cardata5))
  print(i)
}

hist(Accuracy5, 20)
mean(Accuracy5[2:101])
sd(Accuracy5[2:101])



###############################################################################




par(mfrow=c(3,1))
hist(Accuracy, 25)
hist(Accuracy3, 45)
hist(Accuracy4, 65)
