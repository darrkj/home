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

source("Gini.R")
##################################################################
#
#                        Init
#
##################################################################
train <- read.table('training.csv', header=T, sep=",")
grade <- read.table('test.csv', header=T, sep=",")
size = dim(train)[1]

cor(train[17], train[18], na.rm=TRUE, method="pearson")
plot(table(train$x3, train$y)[,1])

train$nil <- NULL
train$x1<- as.Date(train$x1, "%m/%d/%Y")
train$x3 <- NULL #as.factor(train$x3) exact info from x4
train$x18 <- NULL #.99 cor with x17
train$x20 <- NULL #.99 cor with x19
train$x22 <- NULL #.99 cor with x21
train$x24 <- NULL #.99 cor with x23
train$x17 <- as.numeric(train$x17)
train$x19 <- as.numeric(train$x19)
train$x21 <- as.numeric(train$x21)
train$x23 <- as.numeric(train$x23)

ref <- grade$nil
grade$nil <- NULL
grade$x1 <- as.Date(grade$x1, "%m/%d/%Y")
grade$x3 <- NULL #as.factor(train$x3) exact info from x4
grade$x18 <- NULL #.99 cor with x17
grade$x20 <- NULL #.99 cor with x19
grade$x22 <- NULL #.99 cor with x21
grade$x24 <- NULL #.99 cor with x23
grade$x17 <- as.numeric(grade$x17)
grade$x19 <- as.numeric(grade$x19)
grade$x21 <- as.numeric(grade$x21)
grade$x23 <- as.numeric(grade$x23)
##################################################################
#
#                        Break up training data
#
##################################################################

x <- seq(1, size, size/1200)
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
#           Decision Tree Model
#
##################################################################


#can i use leveles from train and grade to make list 
rpart.mod1 <- rpart(y ~ ., data=train1)
rpart.mod2 <- rpart(y ~ ., data=train2)
rpart.mod3 <- rpart(y ~ ., data=train3)
rpart.mod4 <- rpart(y ~ ., data=train4)
rpart.mod5 <- rpart(y ~ ., data=train5)
#plotcp(rpart.mod4)

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
bayes.mod1 <- naiveBayes(y ~ x1+x2+x5+x6, data = train1)
bayes1 <- predict(bayes.mod1, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes1)

bayes.mod2 <- naiveBayes(y ~ x1+x2+x5+x6, data = train2)
bayes2 <- predict(bayes.mod2, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes2)

bayes.mod3 <- naiveBayes(y ~ x1+x2+x5+x6, data = train3)
bayes3 <- predict(bayes.mod3, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes3)

bayes.mod4 <- naiveBayes(y ~ x1+x2+x5+x6, data = train4)
bayes4 <- predict(bayes.mod4, grade, type = c("raw"))[,2]
#Gini(test4$y, bayes4)

bayes.mod5 <- naiveBayes(y ~ x1+x2+x5+x6, data = train5)
bayes5 <- predict(bayes.mod5, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes)

#bayes <- (bayes1 + bayes2 + bayes3 + bayes4 + bayes5)/5
##################################################################
#            Linear Discriminant Analysis Model
#
##################################################################
ldafit1 <- stepclass(y ~ ., data=test1, method="lda", improvement = 0.01)
#ldafit2 <- stepclass(IsBadBuy ~ ., data=train2, method="lda", improvement = 0.01)
#ldafit3 <- stepclass(IsBadBuy ~ ., data=train3, method="lda", improvement = 0.01)
#ldafit4 <- stepclass(IsBadBuy ~ ., data=train4, method="lda", improvement = 0.01)


lda.mod1 <- lda(ldafit1$formula, train1)
lda1 <- predict(lda.mod1, grade)$posterior[,2]
#Gini(test2$y, lda1)



##################################################################
#            Neural Network Model
#
##################################################################

nnet.mod1 <- nnet(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data=train1, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod2 <- nnet(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data=train2, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod3 <- nnet(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data=train3, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod4 <- nnet(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data=train4, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod5 <- nnet(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data=train5, size=3, decay=.1, maxit=1000, MaxNWts=10000)


nnet1 <- predict(nnet.mod1, grade)
nnet2 <- predict(nnet.mod2, grade)
nnet3 <- predict(nnet.mod3, grade)
nnet4 <- predict(nnet.mod4, grade)
nnet5 <- predict(nnet.mod5, grade)

#Gini(test3$y, nnet)
##################################################################
#            vote1 model
#
##################################################################
rand1 <- randomForest(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data = test1)
rand2 <- randomForest(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data = test2)
rand3 <- randomForest(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data = test3)
rand4 <- randomForest(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data = test4)
rand5 <- randomForest(y ~ x1+x2+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x27+x28+x29+x30+x31+x32, data = test5)



fore1 <- predict(rand1, grade)
fore2 <- predict(rand2, grade)
fore3 <- predict(rand3, grade)
fore4 <- predict(rand4, grade)
fore5 <- predict(rand5, grade)

##################################################################
#            Logistic Regession Model
#
#grade$x6[grade$x6 == "2500HD SILVERADO PIC"] <- NA
#xx <-test1[sample(15),]
#drop.levels(xx)
##################################################################
gtrain1 <- train1
gtrain1$x5 <- NULL
gtrain1$x6 <- NULL
glm.mod1 <- glmm(y ~ ., family = binomial(link="logit"), data=gtrain1)
glm.mod1 <- step(glm.mod1)
glm1 <- predict(glm.mod1, data=grade)

gtrain2 <- train2
gtrain2$x5 <- NULL
gtrain2$x6 <- NULL
glm.mod2 <- glmm(y ~ ., family = binomial(link="logit"), data=gtrain2)
glm.mod2 <- step(glm.mod2)
glm2 <- predict.glm(glm.mod2, grade, type="response")

gtrain3 <- train3
gtrain3$x5 <- NULL
gtrain3$x6 <- NULL
glm.mod3 <- glmm(y ~ ., family = binomial(link="logit"), data=gtrain3)
glm.mod3 <- step(glm.mod3)
glm3 <- predict.glm(glm.mod3, grade, type="response")

gtrain4 <- train4
gtrain4$x5 <- NULL
gtrain4$x6 <- NULL
glm.mod4 <- glmm(y ~ ., family = binomial(link="logit"), data=gtrain4)
glm.mod4 <- step(glm.mod4)
glm4 <- predict.glm(glm.mod4, grade, type="response")

gtrain5 <- train5
gtrain5$x5 <- NULL
gtrain5$x6 <- NULL
glm.mod5 <- glmm(y ~ ., family = binomial(link="logit"), data=gtrain5)
glm.mod5 <- step(glm.mod5)
glm5 <- predict.glm(glm.mod5, grade, type="response")

#glm.prob = (glm1 + glm2 + glm3 + glm4 + glm5)/5
#Gini(test5$y, glm.prob)

##################################################################
#            Gereralized Additive Model
#
##################################################################
gam.mod1 <- gam(y ~ s(x1)+x2+s(x4)+s(x13)+x14+x15+x16+s(x17)+s(x18)+s(x19)+s(x20)+s(x21)+s(x22)+s(x23)+s(x24)+s(x28)+x30, family = binomial(link="logit"), data=train1)
gam1 <- predict(gam.mod1, grade)

gam.mod2 <- gam(y ~ s(x1)+x2+s(x4)+s(x13)+x14+x15+x16+s(x17)+s(x18)+s(x19)+s(x20)+s(x21)+s(x22)+s(x23)+s(x24)+s(x28)+x30, family = binomial(link="logit"), data=train2)
gam2 <- predict(gam.mod2, grade)

gam.mod3 <- gam(y ~ s(x1)+x2+s(x4)+s(x13)+x14+x15+x16+s(x17)+s(x18)+s(x19)+s(x20)+s(x21)+s(x22)+s(x23)+s(x24)+s(x28)+x30, family = binomial(link="logit"), data=train3)
gam3 <- predict(gam.mod3, grade)

gam.mod4 <- gam(y ~ s(x1)+x2+s(x4)+s(x13)+x14+x15+x16+s(x17)+s(x18)+s(x19)+s(x20)+s(x21)+s(x22)+s(x23)+s(x24)+s(x28)+x30, family = binomial(link="logit"), data=train4)
gam4 <- predict(gam.mod4, grade)

gam.mod5 <- gam(y ~ s(x1)+x2+s(x4)+s(x13)+x14+x15+x16+s(x17)+s(x18)+s(x19)+s(x20)+s(x21)+s(x22)+s(x23)+s(x24)+s(x28)+x30, family = binomial(link="logit"), data=train5)
gam5 <- predict(gam.mod5, grade)


#Gini(test5$y, gam1)
 
##################################################################
#rpart1 <- predict(rpart.mod1, grade)
#rpart2 <- predict(rpart.mod2, grade)
#rpart3 <- predict(rpart.mod3, grade)
#rpart4 <- predict(rpart.mod4, grade)
#rpart5 <- predict(rpart.mod5, grade)
#glm1 <- predict.glm(glm.mod1, grade, type="response")
#glm2 <- predict.glm(glm.mod2, grade, type="response")
#glm3 <- predict.glm(glm.mod3, grade, type="response")
#glm4 <- predict.glm(glm.mod4, grade, type="response")
#glm5 <- predict.glm(glm.mod5, grade, type="response")
#lda1 <- predict(lda.mod1, grade)$posterior[,2]
#gam1 <- predict(gam.mod1, grade)
#rfprob <- predict(rand, grade)

pp <- data.frame(glm1, glm2, glm3, glm4, glm5,
          gam1, gam2, gam3, gam4, gam5,
          rpart1, rpart2, rpart3, rpart4, rpart5,
          bayes1, bayes2, bayes3, bayes4, bayes5,
          lda1,
          nnet1, nnet2, nnet3, nnet4, nnet5,
          fore1, fore2, fore3, fore4, fore5)

#Gini(test2$y, rfprob)
prob <- rowMeans(pp,na.rm=TRUE)
out <- data.frame(ref, prob)
write.table(prob, file = "data.csv", append = FALSE, quote = FALSE, sep = ", ", 
   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE, 
   qmethod = c("escape", "double"))