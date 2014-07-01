library(testthat)
library(boRg)
library(lubridate)
library(compiler)
adult.train <- read.csv("adulttrain.csv", na.strings="?")
adult.train$y <- as.character(adult.train$y)
adult.train$y <- ifelse(adult.train$y == "<=50K", 0, 1)
size <- nrow(adult.train)
x <- as.factor(rpois(32561, 150))
adult.train$x <- x
#orangeCar.train$RefId <- NULL

#orangeCar.train$PurchDate <- 
#  decimal_date(parse_date_time(orangeCar.train$PurchDate, 
#                  "%m%d%Y", truncated = 3))

n <- ceiling(size * .75)
y <- 1:size
z <- sample(y)
train <- adult.train[z[1:n],]
test <- adult.train[z[(n+1):size],]

rhs <- nrow(train) + nrow(test)
lhs <- nrow(adult.train)
expect_that(lhs == rhs, is_true())

library(rpart)


rmod <- rpart(factor(y) ~ ., 
              train,
              control = rpart.control(minsplit = 2, cp = 0.0007))

plot(rmod)
plotcp(rmod)
zp <- prune(rmod, cp=0.0016)
plot(zp)
text(zp)
pred <- predict(zp, newdata = train, type="prob")
pred1 <- pred[,2]

prd <- ifelse(pred[,2] > .5, 1, 0)
#prd <- ifelse(pred[,2] > mean(train$IsBadBuy)+.2, 1, 0)

Gini(train$y, prd)

sum(train$y == prd) / length(prd)

