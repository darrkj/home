##################################################################
#
#                        Load libraries
#
##################################################################
library(boot)
library(stats)
library(mgcv)
library(rpart)
library(maptree)
library(e1071)
library(MASS)
library(klaR)
library(nnet)
library(randomForest)
require(doSMP)
source("../score.R")
library("arules")
##################################################################
#
#                        Init
#
##################################################################
train <- read.table('training.csv', header=T, sep=",")
grade <- read.table('test.csv', header=T, sep=",")
size = dim(train)[1]
colnames(train)
colnames(grade)

train$nil <- NULL

id <- grade$nil
grade$nil <- NULL
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
  return(map)
}

eval_d <- function(obs, probs) {
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
###############################################################


name <- learn_d(train$x6, train$y)
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

r1 <- eval_d(grade$x6, name)
r2 <- eval_d(grade$x7, desc)
r3 <- eval_d(grade$x8, cap)




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

#Gini(test3$y, nnet)
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

pp <- data.frame(pred1,pred2,pred3,pred4,pred5, r1, r2, r3)
##################################################################

pp <- data.frame(glm1,glm2,glm3,glm4,glm5,
                 gam1,gam2,gam3,gam4,gam5,
                 rpart1,rpart2,rpart3,rpart4,rpart5,
                 bayes1,bayes2,bayes3,bayes4,bayes5,
                 lda1,lda2,lda3,lda4,lda5,
                 nnet1,nnet2,nnet3,nnet4,nnet5,
                 fore1,fore2,fore3,fore4,fore5)

  #bayes1+bayes2+bayes3+bayes4+bayes5+lda+nnet1+nnet2+nnet3+nnet4+nnet5+fore1+fore2+fore3+fore4+fore5)/31


pp <- data.frame(glm1, gam1, rpart1, bayes1, lda1, nnet1, fore1)
pp <- data.frame(r2, r3)
#Gini(test2$y, rfprob)
prob <- rowMeans(pp,na.rm=TRUE)


out <- data.frame(id, prob)
write.table(out, file = "data.csv", append = FALSE, quote = FALSE, sep = ", ", 
   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE, 
   qmethod = c("escape", "double"))
            




##################################################################
#            Logistic Regession Model
#
##################################################################

  glm.mod1 <- glm(y ~ x1+x2+x3+x4+x5, family = binomial(link="logit"), data=train1)
  glm.mod1 <- step(glm.mod1)
  glm1 <- predict.glm(glm.mod1, test1, type="response")

  glm.mod2 <- glm(y ~ x1+x2+x3+x4+x5, family = binomial(link="logit"), data=train2)
  glm.mod2 <- step(glm.mod2)
  glm2 <- predict.glm(glm.mod2, grade, type="response")

  glm.mod3 <- glm(y ~ x1+x2+x3+x4+x5, family = binomial(link="logit"), data=train3)
  glm.mod3 <- step(glm.mod3)
  glm3 <- predict.glm(glm.mod3, grade, type="response")

  glm.mod4 <- glm(y ~ x1+x2+x3+x4+x5, family = binomial(link="logit"), data=train4)
  glm.mod4 <- step(glm.mod4)
  glm4 <- predict.glm(glm.mod4, grade, type="response")

  glm.mod5 <- glm(y ~ x1+x2+x3+x4+x5, family = binomial(link="logit"), data=train5)
  glm.mod5 <- step(glm.mod5)
  glm5 <- predict.glm(glm.mod5, grade, type="response")

 
 
#times <- 50  # times to run the loop
 
# comparing the running time for each loop




#glm.prob = (glm1 + glm2 + glm3 + glm4 + glm5)/5
#Gini(test5$y, glm.prob)

##################################################################
#            Gereralized Additive Model
#
##################################################################
gam.mod1 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5), family = binomial(link="logit"), data=train1)
gam1 <- predict(gam.mod1, grade, type="response")
#plot(gam.mod1,se=T,ask=TRUE)

gam.mod2 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5), family = binomial(link="logit"), data=train2)
gam2 <- predict(gam.mod2, grade, type="response")

gam.mod3 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5), family = binomial(link="logit"), data=train3)
gam3 <- predict(gam.mod3, grade, type="response")

gam.mod4 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5), family = binomial(link="logit"), data=train4)
gam4 <- predict(gam.mod4, grade, type="response")

gam.mod5 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5), family = binomial(link="logit"), data=train5)
gam5 <- predict(gam.mod5, grade, type="response")



#Gini(test5$y, gam1)
      
##################################################################
#           Decision Tree Model
#
##################################################################
rpart.mod1 <- rpart(y ~ x1+x2+x3+x4+x5, data=train1)
rpart.mod2 <- rpart(y ~ x1+x2+x3+x4+x5, data=train2)
rpart.mod3 <- rpart(y ~ x1+x2+x3+x4+x5, data=train3)
rpart.mod4 <- rpart(y ~ x1+x2+x3+x4+x5, data=train4)
rpart.mod5 <- rpart(y ~ x1+x2+x3+x4+x5, data=train5)
#plotcp(rpart.mod1)


#rpart.mod <- prune.rpart(rpart.mod4, cp=.012)
#rpart.mod <- prune.rpart(rpart.mod4, cp=.04)
rpart1 <- predict(rpart.mod1, grade)
rpart2 <- predict(rpart.mod2, grade)
rpart3 <- predict(rpart.mod3, grade)
rpart4 <- predict(rpart.mod4, grade)
rpart5 <- predict(rpart.mod5, grade)

#rpart <- (rpart.prob1 + rpart.prob2 + rpart.prob3 + rpart.prob4 + rpart.prob5)/5

#Gini(test5$IsBadBuy, prob)

#rm(err, cpline, i)
##################################################################
#            Naive Bayes Model
#
##################################################################
bayes.mod1 <- naiveBayes(y ~ x1+x2+x3+x4+x5, data = train1)
bayes1 <- predict(bayes.mod1, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes1)

bayes.mod2 <- naiveBayes(y ~ x1+x2+x3+x4+x5, data = train2)
bayes2 <- predict(bayes.mod2, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes2)

bayes.mod3 <- naiveBayes(y ~ x1+x2+x3+x4+x5, data = train3)
bayes3 <- predict(bayes.mod3, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes3)

bayes.mod4 <- naiveBayes(y ~ x1+x2+x3+x4+x5, data = train4)
bayes4 <- predict(bayes.mod4, grade, type = c("raw"))[,2]
#Gini(test4$y, bayes4)

bayes.mod5 <- naiveBayes(y ~ x1+x2+x3+x4+x5, data = train5)
bayes5 <- predict(bayes.mod5, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes)

#bayes <- (bayes1 + bayes2 + bayes3 + bayes4 + bayes5)/5
##################################################################
#            Linear Discriminant Analysis Model
#
##################################################################
ldafit1 <- stepclass(y ~ x1+x2+x3+x4+x5, data=train1, method="lda", improvement = 0.01)
lda.mod1 <- lda(y~x2, train)
lda1 <- predict(lda.mod1, grade)$posterior[,2]

ldafit2 <- stepclass(y ~ x1+x2+x3+x4+x5, data=test2, method="lda", improvement = 0.01)
lda.mod2 <- lda(y~x1, test2)
lda2 <- predict(lda.mod2, grade)$posterior[,2]

ldafit3 <- stepclass(y ~ x1+x2+x3+x4+x5, data=test3, method="lda", improvement = 0.01)
lda.mod3 <- lda(y~x2, test3)
lda3 <- predict(lda.mod3, grade)$posterior[,2]

ldafit4 <- stepclass(y ~ x1+x2+x3+x4+x5, data=train4, method="lda", improvement = 0.01)
lda.mod4 <- lda(y~x1, test4)
lda4 <- predict(lda.mod4, grade)$posterior[,2]

ldafit5 <- stepclass(y ~ x1+x2+x3+x4+x5, data=train5, method="lda", improvement = 0.01)
lda.mod5 <- lda(y~x1, test5)
lda5 <- predict(lda.mod5, grade)$posterior[,2]

#Gini(test2$y, lda1)