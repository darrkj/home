http://www.kaggle.com/c/DontGetKicked/Data
##################################################################
#                        Load libraries
##################################################################
library(randomForest)
source("score.R")
train <- read.table('cs-training1.csv', header=T, sep=",")
#train <- read.table("sastrain.csv", header=T, sep=",")
#grade <- read.table("cs-test.csv", header=T, sep=",")
size = dim(train)[1]

train$x1 <- as.numeric(train$x1)
train$x2 <- as.numeric(train$x2)
train$x3 <- as.numeric(train$x3)
train$x4 <- as.numeric(train$x4)
train$x5 <- as.numeric(train$x5)
train$x6 <- as.numeric(train$x6)
train$x7 <- as.numeric(train$x7)
train$x8 <- as.numeric(train$x8)
train$x9 <- as.numeric(train$x9)
train$x10 <- as.numeric(train$x10)

x <- seq(1, size, size/6)
y <- 1:size
z <- sample(y)
set1 <- train[z[1:x[2]],]
set2 <- train[z[x[2]:x[3]],]
set3 <- train[z[x[3]:x[4]],]
set4 <- train[z[x[4]:x[5]],]
set5 <- train[z[x[5]:x[6]],]
grade <- train[z[x[6]:size],]
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
#            Random Forest
#
##################################################################  
xx <- c(2,5,10)

RF1 <- randomForest(train1[,-c(xx, 11)], train1$y, data=train1,do.trace=TRUE,importance=TRUE,ntree=50,forest=TRUE)  
pred1 <- predict(RF1,grade[,-c(xx)])
s1 <- AUC(pred1, grade$y)

RF2 <- randomForest(train2[,-c(xx, 11)], train2$y,data=train2,do.trace=TRUE,importance=TRUE,ntree=50,forest=TRUE)  
pred2 <- predict(RF2,grade[,-c(xx)])
s2 <- AUC(pred2, grade$y)

RF3 <- randomForest(train3[,-c(xx, 11)], train3$y,data=train3,do.trace=TRUE,importance=TRUE,ntree=50,forest=TRUE)  
pred3 <- predict(RF3,grade[,-c(xx)])
s3 <- AUC(pred3, grade$y)

RF4 <- randomForest(train4[,-c(xx, 11)], train4$y,data=train4,do.trace=TRUE,importance=TRUE,ntree=50,forest=TRUE)  
pred4 <- predict(RF4,grade[,-c(xx)])
s4 <- AUC(pred4, grade$y)

RF5 <- randomForest(train5[,-c(xx, 11)], train5$y,data=train5,do.trace=TRUE,importance=TRUE,ntree=50,forest=TRUE)  
pred5 <- predict(RF5,grade[,-c(xx)])
s5 <- AUC(pred5, grade$y)


pp <- data.frame(pred1,pred2,pred3,pred4,pred5) 


prob <- rowMeans(pp,na.rm=TRUE)
s <- AUC(prob, grade$y)
c(s, s1, s2, s3, s4, s5)
#out <- data.frame(1:101503, prob)
  
#write.table(out, file = "data.csv", append = FALSE, quote = FALSE, sep = ", ", 
#   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE, 
#   qmethod = c("escape", "double"))