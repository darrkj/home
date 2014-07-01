data <- read.table("data.csv", header=T, sep=",")
size <- dim(data)[1]
#glm
tp.glm <- 0
fp.glm <- 0
tn.glm <- 0
fn.glm <- 0
#gam
tp.gam <- 0
fp.gam <- 0
tn.gam <- 0
fn.gam <- 0
#rpart
tp.rpart <- 0
fp.rpart <- 0
tn.rpart <- 0
fn.rpart <- 0
#bayes
tp.bayes <- 0
fp.bayes <- 0
tn.bayes <- 0
fn.bayes <- 0
#lda
tp.lda <- 0
fp.lda <- 0
tn.lda <- 0
fn.lda <- 0
#nnet
tp.nnet <- 0
fp.nnet <- 0
tn.nnet <- 0
fn.nnet <- 0

tp.vote1 <- 0
fp.vote1 <- 0
tn.vote1 <- 0
fn.vote1 <- 0

tp.vote2 <- 0
fp.vote2 <- 0
tn.vote2 <- 0
fn.vote2 <- 0

tp.vote3 <- 0
fp.vote3 <- 0
tn.vote3 <- 0
fn.vote3 <- 0

tp.vote4 <- 0
fp.vote4 <- 0
tn.vote4 <- 0
fn.vote4 <- 0

real <- data$real
glm <- data$glm
gam <- data$gam
rpart <- data$rpart
bayes <- data$bayes
lda <- data$lda
nnet <- data$nnet

vote1 <- NULL
vote2 <- NULL
vote3 <- NULL
vote4 <- NULL
vote5 <- NULL
vote6 <- NULL

for (i in c(1:size))
{
  sums <- glm[i] + gam[i] + rpart[i] + bayes[i] + lda[i] + nnet[i]
  if (sums == 0)
  {
    vote1[i] <- 0
  } else {
    vote1[i] <- 1
  }
  if (sums == 6)
  {
    vote2[i] <- 1
  } else {
    vote2[i] <- 0
  }
  if (sums == 3)
  {
    vote3[i] <- 1
  } else {
    vote3[i] <- 0
  }
  if (sums == 3)
  {
    vote4[i] <- 0
  } else {
    vote4[i] <- 1
  }
  if (sums > 0)
  {
    vote5[i] <- 1
  }  
  if (sums < 6)
  {
    vote6[i] <- 0
  }  
}



for (i in c(1:size))
{
  if (real[i] == 1)
  {
    if (glm[i] == 1)
    {
      tp.glm <- tp.glm + 1
    } else {
      fn.glm <- fn.glm + 1
    }
    if (gam[i] == 1)
    {
      tp.gam <- tp.gam + 1
    } else {
      fn.gam <- fn.gam + 1
    }
    if (rpart[i] == 1)
    {
      tp.rpart <- tp.rpart + 1
    } else {
      fn.rpart <- fn.rpart + 1
    } 
    if (bayes[i] == 1)
    {
      tp.bayes <- tp.bayes + 1
    } else {
      fn.bayes <- fn.bayes + 1
    } 
    if (lda[i] == 1)
    {
      tp.lda <- tp.lda + 1
    } else {
      fn.lda <- fn.lda + 1
    }
    if (nnet[i] == 1)
    {
      tp.nnet <- tp.nnet + 1
    } else {
      fn.nnet <- fn.nnet + 1
    }    
  }
  else
  {
    if (glm[i] == 0)
    {
      tn.glm <- tn.glm + 1
    } else {
      fp.glm <- fp.glm + 1
    }
    if (gam[i] == 0)
    {
      tn.gam <- tn.gam + 1
    } else {
      fp.gam <- fp.gam + 1
    }
    if (rpart[i] == 0)
    {
      tn.rpart <- tn.rpart + 1
    } else {
      fp.rpart <- fp.rpart + 1
    }
    if (bayes[i] == 0)
    {
      tn.bayes <- tn.bayes + 1
    } else {
      fp.bayes <- fp.bayes + 1
    }
    if (lda[i] == 0)
    {
      tn.lda <- tn.lda + 1
    } else {
      fp.lda <- fp.lda + 1
    }
    if (nnet[i] == 0)
    {
      tn.nnet <- tn.nnet + 1
    } else {
      fp.nnet <- fp.nnet + 1
    }    
  }  
}
 
 
 for (i in c(1:size))
{
  if (real[i] == 1)
  {
    if (vote1[i] == 1)
    {
      tp.vote1 <- tp.vote1 + 1
    } else {
      fn.vote1 <- fn.vote1 + 1
    }
    if (vote2[i] == 1)
    {
      tp.vote2 <- tp.vote2 + 1
    } else {
      fn.vote2 <- fn.vote2 + 1
    }
    if (vote3[i] == 1)
    {
      tp.vote3 <- tp.vote3 + 1
    } else {
      fn.vote3 <- fn.vote3 + 1
    } 
    if (vote4[i] == 1)
    {
      tp.vote4 <- tp.vote4 + 1
    } else {
      fn.vote4 <- fn.vote4 + 1
    }     
  }
  else
  {
    if (vote1[i] == 0)
    {
      tn.vote1 <- tn.vote1 + 1
    } else {
      fp.vote1 <- fp.vote1 + 1
    }
    if (vote2[i] == 0)
    {
      tn.vote2 <- tn.vote2 + 1
    } else {
      fp.vote2 <- fp.vote2 + 1
    }
    if (vote3[i] == 0)
    {
      tn.vote3 <- tn.vote3 + 1
    } else {
      fp.vote3 <- fp.vote3 + 1
    }
    if (vote4[i] == 0)
    {
      tn.vote4 <- tn.vote4 + 1
    } else {
      fp.vote4 <- fp.vote4 + 1
    }   
  }  
}
 
 
glm.recall <- tp.glm/(tp.glm + fn.glm)
glm.prec <- tp.glm/(tp.glm + fp.glm)
glm.sens <- tp.glm/(tp.glm + fn.glm)
glm.spec <- tn.glm/(tn.glm + fp.glm)
glm.acc <- (tp.glm +tn.glm)/(tp.glm + fp.glm + tn.glm + fn.glm)
glm.f <- 2*glm.recall*glm.prec/(glm.recall + glm.prec)

gam.recall <- tp.gam/(tp.gam + fn.gam)
gam.prec <- tp.gam/(tp.gam + fp.gam)
gam.sens <- tp.gam/(tp.gam + fn.gam)
gam.spec <- tn.gam/(tn.gam + fp.gam)
gam.acc <- (tp.gam +tn.gam)/(tp.gam + fp.gam + tn.gam + fn.gam)
gam.f <- 2*gam.recall*gam.prec/(gam.recall + gam.prec)

rpart.recall <- tp.rpart/(tp.rpart + fn.rpart)
rpart.prec <- tp.rpart/(tp.rpart + fp.rpart)
rpart.sens <- tp.rpart/(tp.rpart + fn.rpart)
rpart.spec <- tn.rpart/(tn.rpart + fp.rpart)
rpart.acc <- (tp.rpart +tn.rpart)/(tp.rpart + fp.rpart + tn.rpart + fn.rpart)
rpart.f <- 2*rpart.recall*rpart.prec/(rpart.recall + rpart.prec)

bayes.recall <- tp.bayes/(tp.bayes + fn.bayes)
bayes.prec <- tp.bayes/(tp.bayes + fp.bayes)
bayes.sens <- tp.bayes/(tp.bayes + fn.bayes)
bayes.spec <- tn.bayes/(tn.bayes + fp.bayes)
bayes.acc <- (tp.bayes +tn.bayes)/(tp.bayes + fp.bayes + tn.bayes + fn.bayes)
bayes.f <- 2*bayes.recall*bayes.prec/(bayes.recall + bayes.prec)

lda.recall <- tp.lda/(tp.lda + fn.lda)
lda.prec <- tp.lda/(tp.lda + fp.lda)
lda.sens <- tp.lda/(tp.lda + fn.lda)
lda.spec <- tn.lda/(tn.lda + fp.lda)
lda.acc <- (tp.lda +tn.lda)/(tp.lda + fp.lda + tn.lda + fn.lda)
lda.f <- 2*lda.recall*lda.prec/(lda.recall + lda.prec)

nnet.recall <- tp.nnet/(tp.nnet + fn.nnet)
nnet.prec <- tp.nnet/(tp.nnet + fp.nnet)
nnet.sens <- tp.nnet/(tp.nnet + fn.nnet)
nnet.spec <- tn.nnet/(tn.nnet + fp.nnet)
nnet.acc <- (tp.nnet +tn.nnet)/(tp.nnet + fp.nnet + tn.nnet + fn.nnet)
nnet.f <- 2*nnet.recall*nnet.prec/(nnet.recall + nnet.prec)

vote1.recall <- tp.vote1/(tp.vote1 + fn.vote1)
vote1.prec <- tp.vote1/(tp.vote1 + fp.vote1)
vote1.sens <- tp.vote1/(tp.vote1 + fn.vote1)
vote1.spec <- tn.vote1/(tn.vote1 + fp.vote1)
vote1.acc <- (tp.vote1 +tn.vote1)/(tp.vote1 + fp.vote1 + tn.vote1 + fn.vote1)
vote1.f <- 2*vote1.recall*vote1.prec/(vote1.recall + vote1.prec)

vote2.recall <- tp.vote2/(tp.vote2 + fn.vote2)
vote2.prec <- tp.vote2/(tp.vote2 + fp.vote2)
vote2.sens <- tp.vote2/(tp.vote2 + fn.vote2)
vote2.spec <- tn.vote2/(tn.vote2 + fp.vote2)
vote2.acc <- (tp.vote2 +tn.vote2)/(tp.vote2 + fp.vote2 + tn.vote2 + fn.vote2)
vote2.f <- 2*vote2.recall*vote2.prec/(vote2.recall + vote2.prec)

vote3.recall <- tp.vote3/(tp.vote3 + fn.vote3)
vote3.prec <- tp.vote3/(tp.vote3 + fp.vote3)
vote3.sens <- tp.vote3/(tp.vote3 + fn.vote3)
vote3.spec <- tn.vote3/(tn.vote3 + fp.vote3)
vote3.acc <- (tp.vote3 +tn.vote3)/(tp.vote3 + fp.vote3 + tn.vote3 + fn.vote3)
vote3.f <- 2*vote3.recall*vote3.prec/(vote3.recall + vote3.prec)

vote4.recall <- tp.vote4/(tp.vote4 + fn.vote4)
vote4.prec <- tp.vote4/(tp.vote4 + fp.vote4)
vote4.sens <- tp.vote4/(tp.vote4 + fn.vote4)
vote4.spec <- tn.vote4/(tn.vote4 + fp.vote4)
vote4.acc <- (tp.vote4 +tn.vote4)/(tp.vote4 + fp.vote4 + tn.vote4 + fn.vote4)
vote4.f <- 2*vote4.recall*vote4.prec/(vote4.recall + vote4.prec)

prop <- sum(real)/size


out <- list(c(prop, tp.glm, tn.glm, fp.glm, fn.glm, glm.recall, glm.prec, glm.spec, glm.sens, glm.acc, glm.f,
  tp.gam, tn.gam, fp.gam, fn.gam, gam.recall, gam.prec, gam.spec, gam.sens, gam.acc, gam.f,
  tp.rpart, tn.rpart, fp.rpart, fn.rpart, rpart.recall, rpart.prec, rpart.spec, rpart.sens, rpart.acc, rpart.f,
  tp.bayes, tn.bayes, fp.bayes, fn.bayes, bayes.recall, bayes.prec, bayes.spec, bayes.sens, bayes.acc, bayes.f,
  tp.lda, tn.lda, fp.lda, fn.lda, lda.recall, lda.prec, lda.spec, lda.sens, lda.acc, lda.f,
  tp.nnet, tn.nnet, fp.nnet, fn.nnet, nnet.recall, nnet.prec, nnet.spec, nnet.sens, nnet.acc, nnet.f,
  tp.vote1, tn.vote1, fp.vote1, fn.vote1, vote1.recall, vote1.prec, vote1.spec, vote1.sens, vote1.acc, vote1.f,
  tp.vote2, tn.vote2, fp.vote2, fn.vote2, vote2.recall, vote2.prec, vote2.spec, vote2.sens, vote2.acc, vote2.f,
  tp.vote3, tn.vote3, fp.vote3, fn.vote3, vote3.recall, vote3.prec, vote3.spec, vote3.sens, vote3.acc, vote3.f,
  tp.vote4, tn.vote4, fp.vote4, fn.vote4, vote4.recall, vote4.prec, vote4.spec, vote4.sens, vote4.acc, vote4.f))
  
write.table(out, file = "eval.csv", append = FALSE, quote = FALSE, sep = ", ", 
   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE, 
   qmethod = c("escape", "double"))