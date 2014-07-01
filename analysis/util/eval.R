load("res2.RData")
#source("func.r")
sets <- list.files('C:/Users/Darrell/Dropbox/MSQA/data')
len <- NROW(sets)

#one parameter for all data sets
tmp1 <- "$glm.f"
tmp2 <- "$gam.f"
tmp3 <- "$rpart.f"
tmp4 <- "$bayes.f"
tmp5 <- "$lda.f"
tmp6 <- "$nnet.f"

glm <- NULL
gam <- NULL
rpart <- NULL
bayes <- NULL
lda <- NULL
nnet <- NULL

for (i in c(1:len))
{
  glm[i] <- eval(parse(text=paste(sets[i], tmp1, sep = "")))
  gam[i] <- eval(parse(text=paste(sets[i], tmp2, sep = "")))
  rpart[i] <- eval(parse(text=paste(sets[i], tmp3, sep = "")))
  bayes[i] <- eval(parse(text=paste(sets[i], tmp4, sep = "")))
  lda[i] <- eval(parse(text=paste(sets[i], tmp5, sep = "")))
  nnet[i] <- eval(parse(text=paste(sets[i], tmp6, sep = "")))
}
acc <- data.frame(sets, glm, gam, rpart, bayes, lda, nnet)
write.table(acc, file = "f.csv", append = FALSE, quote = FALSE, sep = ", ", 
   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
   qmethod = c("escape", "double"))

dummy <- c(1:len)

par(mfrow=c(2,3))
plot(dummy, glm, type='p', col="red", main="GLM Accuracy", xlab="Data Set", ylab="Accuracy (%)")
plot(dummy, gam, type='p', col="red", main="GAM Accuracy", xlab="Data Set", ylab="Accuracy (%)")
plot(dummy, rpart, type='p', col="red", main="RPART Accuracy", xlab="Data Set", ylab="Accuracy (%)")
plot(dummy, bayes, type='p', col="red", main="Naive Bayes Accuracy", xlab="Data Set", ylab="Accuracy (%)")
plot(dummy, lda, type='p', col="red", main="LDA Accuracy", xlab="Data Set", ylab="Accuracy (%)")
plot(dummy, nnet, type='p', col="red", main="Neural Network Accuracy", xlab="Data Set", ylab="Accuracy (%)")



