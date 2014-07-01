##################################################################
#
#                        Load libraries
#
##################################################################
library(randomForest)

train <- read.table('cs-training1.csv', header=T, sep=",")
grade <- read.table('cs-test.csv', header=T, sep=",")
size = dim(train)[1]
colnames(train)
colnames(grade)

train$x1 <- as.numeric(train$x1)
train$x2 <- NULL
train$x3 <- as.numeric(train$x3)
train$x4 <- as.numeric(train$x4)
train$x5 <- NULL
train$x6 <- as.numeric(train$x6)
train$x7 <- as.numeric(train$x7)
train$x8 <- as.numeric(train$x8)
train$x9 <- as.numeric(train$x9)
train$x10 <- NULL

grade$x1 <- as.numeric(grade$x1)
grade$x2 <- NULL
grade$x3 <- as.numeric(grade$x3)
grade$x4 <- as.numeric(grade$x4)
grade$x5 <- NULL
grade$x6 <- as.numeric(grade$x6)
grade$x7 <- as.numeric(grade$x7)
grade$x8 <- as.numeric(grade$x8)
grade$x9 <- as.numeric(grade$x9)
grade$x10 <- NULL



x <- seq(1, size, size/5)
y <- 1:size
z <- sample(y)
set1 <- train[z[1:x[2]],]
set2 <- train[z[x[2]:x[3]],]
set3 <- train[z[x[3]:x[4]],]
set4 <- train[z[x[4]:x[5]],]
set5 <- train[z[x[5]:size],]
rm(x, y, z)


train1 <- rbind(set1, set2, set3, set4)
test1 <- set5

train2 <- rbind(set1, set2, set3, set5)
test2 <- set4
  
train3 <- rbind(set1, set2, set4, set5)
test3 <- set3
  
train4 <- rbind(set1, set3, set4, set5)
test4 <- set2
  
train5 <- rbind(set2, set3, set4, set5)
test5 <- set1


rm(set1, set2, set3, set4, set5)

##################################################################
  
  
  
RF1 <- randomForest(y ~ .,data=train1,do.trace=TRUE,importance=TRUE,ntree=500,forest=TRUE)  
pred1 <- predict(RF1,grade)
RF2 <- randomForest(y ~ .,data=train2,do.trace=TRUE,importance=TRUE,ntree=500,forest=TRUE)  
pred2 <- predict(RF2,grade)
RF3 <- randomForest(y ~ .,data=train3,do.trace=TRUE,importance=TRUE,ntree=500,forest=TRUE)  
pred3 <- predict(RF3,grade)
RF4 <- randomForest(y ~ .,data=train4,do.trace=TRUE,importance=TRUE,ntree=500,forest=TRUE)  
pred4 <- predict(RF4,grade)
RF5 <- randomForest(y ~ .,data=train5,do.trace=TRUE,importance=TRUE,ntree=500,forest=TRUE)  
pred5 <- predict(RF5,grade)
pp <- data.frame(pred1,pred2,pred3,pred4,pred5) 


prob <- rowMeans(pp,na.rm=TRUE)

out <- data.frame(1:101503, prob)
  
write.table(out, file = "data.csv", append = FALSE, quote = FALSE, sep = ", ", 
   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE, 
   qmethod = c("escape", "double"))