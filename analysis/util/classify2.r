##################################################################
#
#                        Init
#
##################################################################
library(boot)
library(stats)
load("data.RData")
#Create text file to output data into
sink("output.txt", append=FALSE, split=FALSE)
##################################################################
#
#            Logistic Regession Model
#
##################################################################
mod.glm <- glm(y ~ ., family = binomial(link="logit"), data=train)
summary(mod.glm)
glm.mod <- step(mod.glm)
#summary after stepwise selection
summary(glm.mod)

#use glm model to predict values from the training set
glm.prob <- predict.glm(glm.mod, test, type="response")

#get binary value from pred output
glm.pred <- NULL
glm.correct <- 0
for (i in c(1:size)) 
{ 
    glm.pred[i] <- abs(round(glm.prob[i]))
    if (glm.pred[i] == test$y[i])
    {
        glm.correct <- glm.correct + 1
    }
}

#accuaracy percentage
glm.accuracy <- glm.correct/size
rm(i, mod.glm)
##################################################################
#
#            Gereralized Additive Model
#
##################################################################
library(gam)  #gam model utilities
#construct gam model

gam.mod <- gam(y ~ x1+s(x2)+s(x3)+x4+s(x5)+s(x6)+s(x7)+x8+x9+s(x10)+x11+x12+s(x13)+s(x14), data=train)

summary(gam.mod)

#plot(gam.mod,se=T,ask=T)

gam.prob <- predict(gam.mod, test)

#get binary value from pred output
gam.pred <- NULL
gam.correct <- 0
for (i in c(1:size)) 
{ 
    gam.pred[i] <- round(gam.prob[i])
    if (gam.pred[i] == test$y[i])
    {
        gam.correct <- gam.correct + 1
    }
}

#accuaracy percentage
gam.accuracy <- gam.correct/size
rm(i)
##################################################################
#
#           Decision Tree Model
#
##################################################################
library(rpart) # tree model package
library(maptree) #tree plotting utilities
#rpart model
rpart.mod <- rpart(as.factor(train$y) ~ ., data=train)
summary(rpart.mod)
draw.tree(rpart.mod)

plotcp(rpart.mod)
rpart.model <- prune.rpart(rpart.mod, cp=0.012)
#model after pruning
summary(rpart.mod)
draw.tree(rpart.model)

plot(rpart.model, uniform=TRUE, compress=FALSE, lty=2, branch=0.2)
text(rpart.model, all=TRUE, digits=4, use.n=TRUE, cex=0.5)

rpart.prob <- predict(rpart.model, test, type="prob")[,2]

rpart.pred <- NULL
rpart.correct <- 0
for (i in c(1:size)) 
{ 
    rpart.pred[i] <- round(rpart.prob[i])
    if (rpart.pred[i] == test$y[i])
    {
        rpart.correct <- rpart.correct + 1
    }
}
rpart.accuracy <- rpart.correct/size
rm(i)
##################################################################
#
#            Naive Bayes Model
#
##################################################################

library(e1071)
bayes.mod <- naiveBayes(y ~ ., data = train)
summary(bayes.mod)
bayes.prob <- predict(bayes.mod, test, type = c("raw"))[,2]

bayes.pred <- NULL
bayes.correct <- 0
for (i in c(1:size)) 
{ 
    bayes.pred[i] <- abs(round(bayes.prob[i]))
    if (bayes.pred[i] == test$y[i])
    {
        bayes.correct <- bayes.correct + 1
    }
}
bayes.accuracy <- bayes.correct/size
rm(i)
##################################################################
#
#            Linear Discriminant Analysis Model
#
##################################################################
library(MASS)
library(klaR)
ldafit <- stepclass(y ~ ., data=train, method="lda", improvement = 0.01)
lda.mod <- lda(y ~ x8, train)
summary(lda.mod)

lda.prob <- predict(lda.mod, test)$posterior[,2]
lda.pred <- predict(lda.mod, test)
lda.pred <- as.numeric(unlist(lda.pred[1]))
lda.pred <- lda.pred -1 

lda.correct <- 0
for (i in c(1:size)) 
{ 
    if (lda.pred[[i]] == test$y[i])
    {
        lda.correct <- lda.correct + 1
    }
}
lda.accuracy <- lda.correct/size
rm(i)
##################################################################
#
#            nueral network model
#
##################################################################
library(nnet)

nnet.mod <- nnet(y~.,data=train,size=10,decay=.5,maxit=1000)
summary(nnet.mod)
nnet.prob <- as.numeric(predict(nnet.mod, test))
nnet.pred <- NULL
nnet.correct <- 0
for (i in c(1:size)) 
{ 
    nnet.pred[i] <- round(nnet.prob[i])
    if (nnet.pred[i] == test$y[i])
    {
        nnet.correct <- nnet.correct + 1
    }
}
nnet.accuracy <- nnet.correct/size
rm(i)


##################################################################
#
#            Plot
#
##################################################################

aa <- roc(glm.prob, test$y)
bb <- roc(gam.prob, test$y)
cc <- roc(rpart.prob, test$y)
dd <- roc(bayes.prob, test$y)
ee <- roc(lda.prob, test$y)
ff <- roc(nnet.prob, test$y)

par(mfrow=c(2,3))
plot(aa$fp, aa$tp, type='p', col="red", main="glm", xlab="False Positive", ylab="True Positive")
plot(bb$fp, bb$tp, type='p', col="red", main="gam", xlab="False Positive", ylab="True Positive")
plot(cc$fp, cc$tp, type='p', col="red", main="rpart", xlab="False Positive", ylab="True Positive")
plot(dd$fp, dd$tp, type='p', col="red", main="bayes", xlab="False Positive", ylab="True Positive")
plot(ee$fp, ee$tp, type='p', col="red", main="lda", xlab="False Positive", ylab="True Positive")
plot(ff$fp, ff$tp, type='p', col="red", main="nnet", xlab="False Positive", ylab="True Positive")

dev.copy(png,'roc.png')
dev.off()

glm.auc <- auc(ff$tp, ff$fp)
gam.auc <- auc(ff$tp, ff$fp)
rpart.auc <- auc(ff$tp, ff$fp)
bayes.auc <- auc(ff$tp, ff$fp)
lda.auc <- auc(ff$tp, ff$fp)
nnet.auc <- auc(ff$tp, ff$fp)
    
#(a tp) (b fp) (c tn) (d fn)
tp <- aa$a[50]
fp <- aa$b[50]
tn <- aa$c[50]
fn <- aa$d[50]
glm.spec <- tn/(tn + fp)
glm.prec <- tp/(tp + fp)
glm.rec <- tp/(tp + fn)
glm.acc <- (tp + tn)/(tp + tn + fp + fn)
glm.f <- 2*glm.prec*glm.rec/(glm.rec + glm.prec)
    
tp <- bb$a[50]
fp <- bb$b[50]
tn <- bb$c[50]
fn <- bb$d[50]
gam.spec <- tn/(tn + fp)
gam.prec <- tp/(tp + fp)
gam.rec <- tp/(tp + fn)
gam.acc <- (tp + tn)/(tp + tn + fp + fn)
gam.f <- 2*gam.prec*gam.rec/(gam.rec + gam.prec)
    
tp <- cc$a[50]
fp <- cc$b[50]
tn <- cc$c[50]
fn <- cc$d[50]
rpart.spec <- tn/(tn + fp)
rpart.prec <- tp/(tp + fp)
rpart.rec <- tp/(tp + fn)
rpart.acc <- (tp + tn)/(tp + tn + fp + fn)
rpart.f <- 2*rpart.prec*rpart.rec/(rpart.rec + rpart.prec)
    
tp <- dd$a[50]
fp <- dd$b[50]
tn <- dd$c[50]
fn <- dd$d[50]
bayes.spec <- tn/(tn + fp)
bayes.prec <- tp/(tp + fp)
bayes.rec <- tp/(tp + fn)
bayes.acc <- (tp + tn)/(tp + tn + fp + fn)
bayes.f <- 2*bayes.prec*bayes.rec/(bayes.rec + bayes.prec)
    
tp <- ee$a[50]
fp <- ee$b[50]
tn <- ee$c[50]
fn <- ee$d[50]
lda.spec <- tn/(tn + fp)
lda.prec <- tp/(tp + fp)
lda.rec <- tp/(tp + fn)
lda.acc <- (tp + tn)/(tp + tn + fp + fn)
lda.f <- 2*lda.prec*lda.rec/(lda.rec + lda.prec)
    
tp <- ff$a[50]
fp <- ff$b[50]
tn <- ff$c[50]
fn <- ff$d[50]
nnet.spec <- tn/(tn + fp)
nnet.prec <- tp/(tp + fp)
nnet.rec <- tp/(tp + fn)
nnet.acc <- (tp + tn)/(tp + tn + fp + fn)
nnet.f <- 2 * nnet.prec * nnet.rec/(nnet.rec + nnet.prec)


##################################################################
#
#            Relavent Values
#
##################################################################

australia <- list(glm.acc=glm.acc, glm.auc=glm.auc, glm.spec=glm.spec, 
      glm.prec=glm.prec, glm.f=glm.f, glm.rec=glm.rec, 
      glm.pred=glm.pred, glm.prob=glm.prob, glm.mod=glm.mod,
      glm.tp=aa$a, glm.fp=aa$b, glm.tn=aa$c, glm.fn=aa$d,
    gam.acc=gam.acc, gam.auc=gam.auc, gam.spec=gam.spec, 
      gam.prec=gam.prec, gam.f=gam.f, gam.rec=gam.rec, 
      gam.pred=gam.pred, gam.prob=gam.prob, gam.mod=gam.mod, 
      gam.tp=bb$a, gam.fp=bb$b, gam.tn=bb$c, gam.fn=bb$d,
    rpart.acc=rpart.acc, rpart.auc=rpart.auc, rpart.spec=rpart.spec, 
      rpart.prec=rpart.prec, rpart.f=rpart.f, rpart.rec=rpart.rec, 
      rpart.pred=rpart.pred, rpart.prob=rpart.prob, rpart.mod=rpart.mod, 
      rpart.tp=cc$a, rpart.fp=cc$b, rpart.tn=cc$c, rpart.fn=cc$d,
    bayes.acc=bayes.acc, bayes.auc=bayes.auc, bayes.spec=bayes.spec, 
      bayes.prec=bayes.prec, bayes.f=bayes.f, bayes.rec=bayes.rec, 
      bayes.pred=bayes.pred, bayes.prob=bayes.prob, bayes.mod=bayes.mod, 
      bayes.tp=dd$a, bayes.fp=dd$b, bayes.tn=dd$c, bayes.fn=dd$d,
    lda.acc=lda.acc, lda.auc=lda.auc, lda.spec=lda.spec, 
      lda.prec=lda.prec, lda.f=lda.f, lda.rec=lda.rec, 
      lda.pred=lda.pred, lda.prob=lda.prob, lda.mod=lda.mod, 
      lda.tp=ee$a, lda.fp=ee$b, lda.tn=ee$c, lda.fn=ee$d,
    nnet.acc=nnet.acc, nnet.auc=nnet.auc, nnet.spec=nnet.spec, 
      nnet.prec=nnet.prec, nnet.f=nnet.f, nnet.rec=nnet.rec, 
      nnet.pred=nnet.pred, nnet.prob=nnet.prob, nnet.mod=nnet.mod, 
      nnet.tp=ff$a, nnet.fp=ff$b, nnet.tn=ff$c, nnet.fn=ff$d,
    y=test$y, prop=trues, trues=trues, falses=falses, obs=length(test$y))

##################################################################
#
#            Print results file
#
##################################################################

outdata=data.frame(glm=glm.pred, gam=gam.pred, rpart=rpart.pred, 
   bayes=bayes.pred, lda=lda.pred, nnet=nnet.pred, real=test$y)
write.table(outdata, file = "data.csv", append = FALSE, quote = FALSE, sep = ", ", 
   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
   qmethod = c("escape", "double"))

save(file="workspace.RData")

rm(aa, bb, cc, data, dd, ee, ff, outdata, test, train)
rm(bayes.acc, bayes.accuracy, bayes.correct, bayes.f, bayes.mod, bayes.pred, bayes.auc)
rm(bayes.prec, bayes.prob, bayes.spec, bayes.rec)
rm(glm.acc, glm.accuracy, glm.correct, glm.f, glm.mod, glm.pred, glm.auc)
rm(glm.prec, glm.prob, glm.spec, glm.rec)
rm(gam.acc, gam.accuracy, gam.correct, gam.f, gam.mod, gam.pred, gam.auc)
rm(gam.prec, gam.prob, gam.spec, gam.rec)
rm(rpart.acc, rpart.accuracy, rpart.correct, rpart.f, rpart.mod, rpart.pred, rpart.auc)
rm(rpart.prec, rpart.prob, rpart.spec, rpart.rec)
rm(lda.acc, lda.accuracy, lda.correct, lda.f, lda.mod, lda.pred, lda.auc)
rm(lda.prec, lda.prob, lda.spec, lda.rec)
rm(nnet.acc, nnet.accuracy, nnet.correct, nnet.f, nnet.mod, nnet.pred, nnet.auc)
rm(nnet.prec, nnet.prob, nnet.spec, nnet.rec)
rm(bds, falses, fn, tp, fp, tn, ldafit, len, prop, rpart.model)
rm(num.mv, size, trues)
