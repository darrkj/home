##################################################################
#                        Init
#
##################################################################
library(boot)
library(stats)
library(gam)
library(rpart)
library(maptree)
library(e1071)
library(MASS)
library(klaR)
source("func.r")
gamfunction <- y ~ x1+s(x2)+s(x3)+x4+s(x5)+s(x6)+s(x7)+x8+x9+s(x10)+x11+x12+s(x13)+s(x14)
filename <- 'australian.csv'    
#Create text file to output data into
#sink("output.txt", append=FALSE, split=FALSE)
data <- read.table(filename, header=T, sep=",")
size <- dim(data)[1];

num.mv <- sum(is.na(data))
data <- data[complete.cases(data),]
prop <- table(data$y)

trues <- prop[[2]]/size
falses <- prop[[1]]/size

x <- seq(1, size, size/5)
y <- 1:size
z <- sample(y)
set1 <- data[z[1:x[2]],]
set2 <- data[z[x[2]:x[3]],]
set3 <- data[z[x[3]:x[4]],]
set4 <- data[z[x[4]:x[5]],]
set5 <- data[z[x[5]:size],]
rm(x, y, z)

for (j in 1:5)
{
  if (j == 1)
  {
    train <- rbind(set1, set2, set3, set4)
    test <- set5
  }
  else if (j == 2)
  {
    train <- rbind(set1, set2, set3, set5)
    test <- set4
  }
  else if (j == 3)
  {
    train <- rbind(set1, set2, set4, set5)
    test <- set3
  }
  if (j == 4)
  {
    train <- rbind(set1, set3, set4, set5)
    test <- set2
  }
  if (j == 5)
  {
    train <- rbind(set2, set3, set4, set5)
    test <- set1
  }
  ##################################################################
  #            Logistic Regession Model
  #
  ##################################################################
  mod.glm <- glm(y ~ ., family = binomial(link="logit"), data=train)
  glm.mod <- step(mod.glm)
  #use glm model to predict values from the training set
  glm.prob <- predict.glm(glm.mod, test, type="response")
  rm(mod.glm)
  ##################################################################
  #            Gereralized Additive Model
  #
  ##################################################################
  gam.mod <- gam(gamfunction, data=train)
  
  #plot(gam.mod,se=T,ask=T)
  gam.prob <- predict(gam.mod, test)
  ##################################################################
  #           Decision Tree Model
  #
  ##################################################################
  rpart.mod <- rpart(as.factor(train$y) ~ ., data=train)
  
  err <- rpart.mod[[5]][,4]
  cpline <- sum(err)/length(err)
  i <- 1
  repeat
  {
    if (err[i] < cpline) {break}
    i <- i + 1
  }
  rpart.mod <- prune.rpart(rpart.mod, cp=rpart.mod[[5]][i,1])
  rpart.prob <- predict(rpart.mod, test)[,2]
  rm(err, cpline, i)
  ##################################################################
  #            Naive Bayes Model
  #
  ##################################################################
  bayes.mod <- naiveBayes(y ~ ., data = train)
  bayes.prob <- predict(bayes.mod, test, type = c("raw"))[,2]
  
  ##################################################################
  #            Linear Discriminant Analysis Model
  #
  ##################################################################
  ldafit <- stepclass(y ~ ., data=train, method="lda", improvement = 0.01)
  lda.mod <- lda(ldafit$formula, train)
  
  lda.prob <- predict(lda.mod, test)$posterior[,2]
  rm(ldafit)
  ##################################################################
  #            vote1 model
  #
  ##################################################################
  vote1.prob <- (glm.prob + gam.prob + rpart.prob + bayes.prob + lda.prob)/5
  ##################################################################
  #            vote2 model
  #
  ##################################################################
  vote2.prob <- (round(glm.prob) + round(gam.prob) + round(rpart.prob) + round(bayes.prob) + round(lda.prob))/5
  ##################################################################
  #            scoring
  #
  ##################################################################
  glm   <- score(glm.prob,   test$y)
  gam   <- score(gam.prob,   test$y)
  rpart <- score(rpart.prob, test$y)
  bayes <- score(bayes.prob, test$y)
  lda   <- score(lda.prob,   test$y)
  vote1 <- score(vote1.prob, test$y)
  vote2 <- score(vote2.prob, test$y)
  
  if (j == 1)
  {
    glm.tot   <- glm
    gam.tot   <- gam
    rpart.tot <- rpart
    bayes.tot <- bayes
    lda.tot   <- lda
    vote1.tot <- vote1
    vote2.tot <- vote2
  }
  else
  {
    glm.tot   <- glm.tot + glm
    gam.tot   <- gam.tot + gam
    rpart.tot <- rpart.tot + rpart
    bayes.tot <- bayes.tot + bayes
    lda.tot   <- lda.tot + lda
    vote1.tot <- vote1.tot + vote1
    vote2.tot <- vote2.tot + vote2
  }
  rm(bayes, glm, gam, rpart, lda, vote1, vote2)
  rm(train, test, gam.prob, gam.mod)
  rm(glm.mod, glm.prob, rpart.mod, rpart.prob)
  rm(lda.mod, lda.prob, bayes.prob, bayes.mod)
  rm(vote1.prob, vote2.prob)
}
##################################################################
#
#            Relavent Values
#
##################################################################


glm   <- glm.tot/5
gam   <- gam.tot/5
rpart <- rpart.tot/5
bayes <- bayes.tot/5
lda   <- lda.tot/5
vote1 <- vote1.tot/5
vote2 <- vote2.tot/5

res <- data.frame(glm, gam, rpart, bayes, lda, vote1, vote2)

rm(glm.tot, gam.tot, rpart.tot, bayes.tot, lda.tot, vote1.tot, vote2.tot)
rm(data, set1,set2, set3, set4, set5)
rm(falses, prop, num.mv, size, trues, j)
rm(glm, gam, rpart, bayes, lda, vote1, vote2)
rm(auc, rate, roc, score, filename, gamfunction)

australian <- res
save(australian, file="australian.RData")
rm(australian, res)