##################################################################
#
#            Functions
#
##################################################################

rate <- function(prob, y) 
{
  len <- length(prob)
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  for (j in c(0:100)) 
  {
    thresh <- j/100
    a <- 0
    b <- 0
    c <- 0
    d <- 0
    for (i in c(1:len)) 
    {
      if (prob[i] > thresh) 
      {
        pred <- 1
      } else 
      {
        pred <- 0
      }
      if ( y[i] == 1 && pred == 1) 
      {
        a <- a + 1
      }
      if ( y[i] == 0 && pred == 0) 
      {
        b <- b + 1
      }
      if ( y[i] == 1 && pred == 0) 
      {
        d <- d + 1
      }
      if ( y[i] == 0 && pred == 1) 
      {
        c <- c + 1
      }
    }
    tp[j+1] <- a
    tn[j+1] <- b
    fp[j+1] <- c
    fn[j+1] <- d
  }
  result <- data.frame(tp, fp, tn, fn)
  return(result)
}
  

roc <- function(rate) 
{
  tp <- rate$tp
  fp <- rate$fp
  tn <- rate$tn
  fn <- rate$fn
  len <- length(tp)
  t <- NULL
  f <- NULL
  for (i in c(1:len))
  {
    t[i] <- tp[i]/(tp[i]+fn[i])
    f[i] <- 1 - (tn[i]/(tn[i] + fp[i]))
  }
  result <- data.frame(t, f)
  return(result)
}


auc <- function(roc) 
{
  tp <- roc$tp
  fp <- roc$fp
  len <- length(tp)
  sums <- NULL
  for (i in c(1:100))
  {
    j <- 101-i
    sums[i] <- abs((fp[j+1]-fp[j]))*(tp[j+1]+tp[j])/2
  }
  sums <- sum(sums)
  return(sums)
}

score <- function(prob, y)
{
  rate <- rate(prob, y)
  roc <- roc(rate)
  auc <- auc(roc)
  tp <- rate$tp[50]
  fp <- rate$fp[50]
  tn <- rate$tn[50]
  fn <- rate$fn[50]
  spec <- tn/(tn + fp)
  prec <- tp/(tp + fp)
  rec <- tp/(tp + fn)
  acc <- (tp + tn)/(tp + tn + fp + fn)
  f <- 2*prec*rec/(rec + prec) 
  result <- c(roc, auc=auc, spec=spec, prec=prec, rec=rec, acc=acc, f=f)
  return(result)
}