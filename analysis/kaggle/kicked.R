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
##################################################################
#
#                        Create Functions
#
##################################################################
 
##################################################################
#
#                        Init
#
##################################################################
train <- read.table('cs-training1.csv', header=T, sep=",")
grade <- read.table('cs-test.csv', header=T, sep=",")
size = dim(train)[1]
colnames(train)
colnames(grade)

train$x1 <- as.numeric(train$x1)
train$x2 <- as.numeric(train$x2)
train$x3 <- as.numeric(train$x3)
train$x4 <- as.numeric(train$x4)
train$x5 <- as.numeric(train$x5)
train$x6 <- as.numeric(train$x6)
train$x7 <- NULL
train$x8 <- as.numeric(train$x8)
train$x9 <- NULL
train$x10 <- as.numeric(train$x10)



cor(train, use="complete", method="pearson")
plot(table(train$x3, train$y)[,1])

grade$x1 <- as.numeric(grade$x1)
grade$x2 <- as.numeric(grade$x2)
grade$x3 <- as.numeric(grade$x3)
grade$x4 <- as.numeric(grade$x4)
grade$x5 <- as.numeric(grade$x5)
grade$x6 <- as.numeric(grade$x6)
grade$x7 <- NULL
grade$x8 <- as.numeric(grade$x8)
grade$x9 <- NULL
grade$x10 <- as.numeric(grade$x10)



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
#            Logistic Regession Model
#
##################################################################
glm.mod1 <- glm(y ~ ., family = binomial(link="logit"), data=test1)
glm.mod1 <- step(glm.mod1)
glm1 <- predict.glm(glm.mod1, grade, type="response")

glm.mod2 <- glm(y ~ ., family = binomial(link="logit"), data=test2)
glm.mod2 <- step(glm.mod2)
glm2 <- predict.glm(glm.mod1, grade, type="response")

glm.mod3 <- glm(y ~ ., family = binomial(link="logit"), data=test3)
glm.mod3 <- step(glm.mod3)
glm3 <- predict.glm(glm.mod1, grade, type="response")

glm.mod4 <- glm(y ~ ., family = binomial(link="logit"), data=test4)
glm.mod4 <- step(glm.mod4)
glm4 <- predict.glm(glm.mod1, grade, type="response")

glm.mod5 <- glm(y ~ ., family = binomial(link="logit"), data=test5)
glm.mod5 <- step(glm.mod5)
glm5 <- predict.glm(glm.mod1, grade, type="response")

#glm.prob = (glm1 + glm2 + glm3 + glm4 + glm5)/5
#Gini(test5$y, glm.prob)

##################################################################
#            Gereralized Additive Model
#
##################################################################
gam.mod1 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x8)+s(x10), family = binomial(link="logit"), data=train1)
gam1 <- predict(gam.mod1, grade)
#plot(gam.mod1,se=T,ask=TRUE)

gam.mod2 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x8)+s(x10), family = binomial(link="logit"), data=train2)
gam2 <- predict(gam.mod2, grade)

gam.mod3 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x8)+s(x10), family = binomial(link="logit"), data=train3)
gam3 <- predict(gam.mod3, grade)

gam.mod4 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x8)+s(x10), family = binomial(link="logit"), data=train4)
gam4 <- predict(gam.mod4, grade)

gam.mod5 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x8)+s(x10), family = binomial(link="logit"), data=train5)
gam5 <- predict(gam.mod5, grade)



#Gini(test5$y, gam1)
      
##################################################################
#           Decision Tree Model
#
##################################################################
rpart.mod1 <- rpart(y ~ ., data=train1)
rpart.mod2 <- rpart(y ~ ., data=train2)
rpart.mod3 <- rpart(y ~ ., data=train3)
rpart.mod4 <- rpart(y ~ ., data=train4)
rpart.mod5 <- rpart(y ~ ., data=train5)
#plotcp(rpart.mod2)


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
bayes.mod1 <- naiveBayes(y ~ ., data = train1)
bayes1 <- predict(bayes.mod1, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes1)

bayes.mod2 <- naiveBayes(y ~ ., data = train2)
bayes2 <- predict(bayes.mod2, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes2)

bayes.mod3 <- naiveBayes(y ~ ., data = train3)
bayes3 <- predict(bayes.mod3, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes3)

bayes.mod4 <- naiveBayes(y ~ ., data = train4)
bayes4 <- predict(bayes.mod4, grade, type = c("raw"))[,2]
#Gini(test4$y, bayes4)

bayes.mod5 <- naiveBayes(y ~ ., data = train5)
bayes5 <- predict(bayes.mod5, grade, type = c("raw"))[,2]
#Gini(test1$y, bayes)

#bayes <- (bayes1 + bayes2 + bayes3 + bayes4 + bayes5)/5
##################################################################
#            Linear Discriminant Analysis Model
#
##################################################################
ldafit1 <- stepclass(y ~ ., data=test1, method="lda", improvement = 0.01)
lda.mod1 <- lda(ldafit1, test1)
lda1 <- predict(lda.mod1, grade)$posterior[,2]

ldafit2 <- stepclass(y ~ ., data=test2, method="lda", improvement = 0.01)
lda.mod2 <- lda(ldafit2, test2)
lda2 <- predict(lda.mod2, grade)$posterior[,2]

ldafit3 <- stepclass(y ~ ., data=test3, method="lda", improvement = 0.01)
lda.mod3 <- lda(ldafit3, test3)
lda3 <- predict(lda.mod3, grade)$posterior[,2]

ldafit4 <- stepclass(y ~ ., data=train4, method="lda", improvement = 0.01)
lda.mod4 <- lda(ldafit4, test4)
lda4 <- predict(lda.mod4, grade)$posterior[,2]

ldafit5 <- stepclass(y ~ ., data=train5, method="lda", improvement = 0.01)
lda.mod5 <- lda(ldafit5, test5)
lda5 <- predict(lda.mod5, grade)$posterior[,2]

#Gini(test2$y, lda1)
##################################################################
#            Neural Network Model
#
##################################################################

nnet.mod1 <- nnet(y ~ ., data=train1, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod2 <- nnet(y ~ ., data=train2, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod3 <- nnet(y ~ ., data=train3, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod4 <- nnet(y ~ ., data=train4, size=3, decay=.1, maxit=1000, MaxNWts=10000)
nnet.mod5 <- nnet(y ~ ., data=train5, size=3, decay=.1, maxit=1000, MaxNWts=10000)


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
rand1 <- randomForest(y ~ ., data = test1)
rand2 <- randomForest(y ~ ., data = test2)
rand3 <- randomForest(y ~ ., data = test3)
rand4 <- randomForest(y ~ ., data = test4)
rand5 <- randomForest(y ~ ., data = test5)



fore1 <- predict(rand1, grade)
fore2 <- predict(rand2, grade)
fore3 <- predict(rand3, grade)
fore4 <- predict(rand4, grade)
fore5 <- predict(rand5, grade)
##################################################################

pp <- data.frame(glm1,glm2,glm3,glm4,glm5,
                 rpart1,rpart2,rpart3,rpart4,rpart5,
                 bayes1,bayes2,bayes3,bayes4,bayes5,
                 lda1,lda2,lda3,lda4,lda5,
                 nnet1,nnet2,nnet3,nnet4,nnet5)

  #bayes1+bayes2+bayes3+bayes4+bayes5+lda+nnet1+nnet2+nnet3+nnet4+nnet5+fore1+fore2+fore3+fore4+fore5)/31

#Gini(test2$y, rfprob)
prob <- rowMeans(pp,na.rm=TRUE)

out <- data.frame(1:101503, prob)
write.table(out, file = "data.csv", append = FALSE, quote = FALSE, sep = ", ", 
   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE, 
   qmethod = c("escape", "double"))