##################################################################
#
#                        Load libraries
#
##################################################################
library(nnet)
library(randomForest)
source("../score.R")
##################################################################
#
#                        Init
#
##################################################################
data <- read.table('training.csv', header=T, sep=",")
grade <- read.table('test.csv', header=T, sep=",")

size = floor(dim(data)[1] * .8)
bds<-sample(rownames(data),size)
train <- data[bds,]
test <- data[setdiff(rownames(data), bds),]


train$nil <- NULL
test$nil <- NULL

id <- grade$nil
grade$nil <- NULL

def <- 0.26277777

name_tr <- NULL
desc_tr <- NULL
cap_tr  <- NULL
text_tr <- NULL
for (i in 1:NROW(train)) {
  name_tr <- c(name_tr, strsplit(as.character(train$x6[i]), "\\ "))
  desc_tr <- c(desc_tr, strsplit(as.character(train$x7[i]), "\\ "))
  cap_tr  <- c(cap_tr, strsplit(as.character(train$x8[i]), "\\ "))
  tmp <- list(unique(c(unlist(name_tr[i]), unlist(desc_tr[i]), unlist(cap_tr[i]))))
  text_tr <- c(text_tr, tmp)
}

name_te <- NULL
desc_te <- NULL
cap_te  <- NULL
text_te <- NULL
for (i in 1:NROW(test)) {
  name_te <- c(name_te, strsplit(as.character(test$x6[i]), "\\ "))
  desc_te <- c(desc_te, strsplit(as.character(test$x7[i]), "\\ "))
  cap_te  <- c(cap_te, strsplit(as.character(test$x8[i]), "\\ "))
  tmp <- list(unique(c(unlist(name_te[i]), unlist(desc_te[i]), unlist(cap_te[i]))))
  text_te <- c(text_te, tmp)
}

name_gr <- NULL
desc_gr <- NULL
cap_gr  <- NULL
text_gr <- NULL
for (i in 1:NROW(grade)) {
  name_gr <- c(name_gr, strsplit(as.character(grade$x6[i]), "\\ "))
  desc_gr <- c(desc_gr, strsplit(as.character(grade$x7[i]), "\\ "))
  cap_gr  <- c(cap_gr, strsplit(as.character(grade$x8[i]), "\\ "))
  tmp <- list(unique(c(unlist(name_gr[i]), unlist(desc_gr[i]), unlist(cap_gr[i]))))
  text_gr <- c(text_gr, tmp)
}

rm(bds)
##################################################################
#
#                        Create Functions
#
##################################################################
learn_d <- function(desc, y) {
  map1 <- NULL
  map0 <- NULL
  for (i in 0:2500) {
    map1[[as.character(i)]] <- 0
    map0[[as.character(i)]] <- 0
  }
  for (i in 1:size){
    xx <- as.character(desc[i])
    yy <- unlist(strsplit(xx, "\\ "))
    len <- NROW(yy)
    if (len > 0) {
      for ( j in 1:len){
        if (y[i] == 1) {
          map1[[yy[j]]] <- map1[[yy[j]]] + 1
        }
        if (y[i] == 0) {
          map0[[yy[j]]] <- map0[[yy[j]]] + 1
        }
      }
    }
  }
  map <- data.frame(map1, map0)
}

eval_ <- function(obs, probs) {
  outcome <- NULL
  num <- length(obs)
  for (i in 1:num) {
    xx <- as.character(obs[i])
    yy <- unlist(strsplit(xx, "\\ "))
    len <- NROW(yy)
    accum <- 0
    if (len == 0) {
      pred <- .5
    } else {
      for (j in 1:len) {
        desc <- as.numeric(yy[j]) + 1
        accum <- accum + probs[desc,1]/(probs[desc,1] + probs[desc,2])
        pred <- accum/j
      }
    }
    outcome[i] <- pred
  }
  return(outcome)
}

xx <- NULL
for (i in 1:NROW(train)) {
  xx <- c(xx, strsplit(as.character(train$x7[i]), "\\ "))
}
list1 <- as.numeric(unlist(strsplit(as.character(x6[9987]), "\\ ")))
list2 <- as.list(high)
int <- mapply(function(x, y) any(x %in% y), x1, y1)
any(TRUE %in% int)
  
  
eval_c <- function(obs, high) {
  outcome <- NULL
  list2 <- as.list(high)
  num <- length(obs)
  for (i in 1:num) {
    list1 <- as.numeric(unlist(strsplit(as.character(obs[i]), "\\ ")))
    if (length(list1) == 0) {
      pred <- .15
    } else {
      int <- mapply(function(x, y) any(x %in% y), list1, list2)
      if (any(TRUE %in% int)){
        pred <- .9
      } else {
        pred <- .1
      } 
    }
    outcome[i] <- pred
  }
  return(outcome)
}
###############################################################


name <- learn_d(text_tr, train$y)
train$x6 <- NULL
desc <- learn_d(train$x7, train$y)
train$x7 <- NULL
cap <- learn_d(train$x8, train$y)
train$x8 <- NULL

x6 <- grade$x6
grade$x6 <- NULL
x7 <- grade$x7
grade$x7 <- NULL
x8 <- grade$x8
grade$x8 <- NULL

r1 <- eval_c(x6, name)
r2 <- eval_c(x7, desc)
r3 <- eval_c(x8, cap)

s1 <- eval_d(x6, name)
s2 <- eval_d(x7, desc)
s3 <- eval_d(x8, cap)










##################################################################
#            Evaluate
#
##################################################################



pp <- data.frame(pred1,pred2,pred3,pred4,pred5, nnet1, nnet2, nnet3, nnet4, nnet5)
prob <- rowMeans(pp,na.rm=TRUE)
pp <- data.frame(r1, r2, r3)


prob <- rowMeans(pp,na.rm=TRUE)


out <- data.frame(id, prob, r1, r2, r3)
write.table(out, file = "data.csv", append = FALSE, quote = FALSE, sep = ", ", 
   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE, 
   qmethod = c("escape", "double"))



##################################################################
#            Neural Network Model
#
##################################################################

nnet.mod1 <- nnet(y ~ x1+x2+x3+x4+x5, data=train1, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod2 <- nnet(y ~ x1+x2+x3+x4+x5, data=train2, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod3 <- nnet(y ~ x1+x2+x3+x4+x5, data=train3, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod4 <- nnet(y ~ x1+x2+x3+x4+x5, data=train4, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod5 <- nnet(y ~ x1+x2+x3+x4+x5, data=train5, size=3, decay=.1, maxit=1000, MaxNWts=10000)


nnet1 <- predict(nnet.mod1, grade)
nnet2 <- predict(nnet.mod2, grade)
nnet3 <- predict(nnet.mod3, grade)
nnet4 <- predict(nnet.mod4, grade)
nnet5 <- predict(nnet.mod5, grade)
##################################################################
#            Random Forest
#
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
##################################################################



        