###############################################################################
#
#                                   scores.R
#
###############################################################################
#
# Author - Kenny Darrell
#
# These are functions related to scoring, mostly for supervised model results
# Most of this code was stolen from the methods used to evaluate Kaggle 
# competitions. 
#
###############################################################################

###############################################################################
#' Percent of complete rows 
#'
#' This displays the percentage of \code{\link{complete.cases}}
#'
#' @author Kenny Darrell
#' @return data.frame
#' 
#' @param df data frame under consideration
#' 
#' @keywords info
#' @export
#' @examples
#' data(orangeCar.test)
#' compRow(orangeCar.test)
#'
# a is a vector of observed scores
# p is a vector of predictions
# w is a vector of weights
wmae <- function(a, p, w) {
  return(sum(abs(a-p)*w)/sum(w))
}
# TODO: data: Need to add orange car data
###
 #  Gini function from Kaggle kicked/carvan competion.
 #  It takes the actual binary outcome, a vector of zeros and ones
 #  as well as the probability, a vector a values ranging from zero to one.
 #  a = actual values
 #  p = predicted probabilities
 # 
 #  Example usage
 #
 #  > x <- c(0, 1, 0)
 #  > y <- c(.2, .8, .1)
 #  > Gini(x, y)
 #  [1] 0.3333333
 #
 ##

Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range = c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a))
    accum.losses <- temp.df$actual / total.losses
    gini.sum <- cumsum(accum.losses - null.losses)
    sum(gini.sum) / length(a)
  }

###
 #  Area Under the Curve function form the kaggle 
 #  credit competion.
 #  it takes.
 #
 ##

AUC <- function(x, y) 
{
  if (!is.numeric(x)) stop("'x' must be numeric")
  if (length(x) != length(y)) stop("length(y) and length(x) must be the same")
  
  y <- as.factor(y); nY <- table(y)
  if(nlevels(y) != 2) stop("'y' must have two levels")
  n1 <- nY[1]; n2 <- nY[2]
  uL <- as.factor(levels(y))
  r <- rank(c(x[which(y == uL[1])], x[which(y == uL[2])]))
  auc <- as.double((sum(r[1:n1]) - n1 * (n1 + 1) / 2) / (n1 * n2))
  max(auc,1-auc)
}

###
 #  Caped Binomial Deviance Function, for the Kaggle photo competion.
 #
 #  Example usage
 #
 #  > x <- c(0, 1, 0)
 #  > y <- c(.2, .8, .1)
 #  > capdev(x, y)
 #  [1] 0.07985917
 #
 ##

capdev <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    p_capped <- pmin(0.99, p)
    p_capped <- pmax(0.01, p_capped)
    -sum(a * log(p_capped, base=10) + (1 - a) * log(1 - p_capped, base=10)) / length(a)
}

###
 #  Scoring a metric
 #
 #  Example usage
 #
 #  n <- score(orangeCar.train, 26, 0, 10, 1000, 2500)
 #
 #  This will result in a replicated dataset with a 
 #  new parameter having the same name as the scored 
 #  parameter appended with "_score".  A plot is also 
 #  generated with displaying the mapping.
 ##

score <- function(dataSet, field, min=0, max=10, floor, ceil) {
  name <- paste(names(dataSet)[field], "score", sep="_")
  
  score <- (((dataSet[,field] - floor) * (max - min)) / (ceil - floor)) + min
  score <- ifelse(score > max, max, score)
  score <- ifelse(score < min, min, score)
  
  dataSet[,ncol(dataSet)+1] <- score
  names(dataSet)[ncol(dataSet)] <- name
  
  plot(dataSet[,field], dataSet[,ncol(dataSet)],
       xlab = eval(names(dataSet)[field]), 
       ylab = eval(names(dataSet)[ncol(dataSet)])
  )
  #add defualt to not plot, ex plot = 1
  return (dataSet)
}

params <- function(a, p, cutoff=.5) {
  p1 <- ifelse(p > cutoff, 1, 0)
  tp <- sum(a & p1)
  tn <- sum(!a & !p1)
  fp <- sum(!a & p1)
  fn <- sum(a & !p1)
  
  return(list(tp=tp, fp=fp, fn=fn, tn=tn))
}


roc <- function(a, p, gran=.1) {
  s1 <- rep(NA, (1/gran)+1)
  s2 <- rep(NA, (1/gran)+1)
  for (i in seq(0, 1, gran)) {
    x <- params(a, p, cutoff=i) 
    s1[i] <- (x$tp / (x$tp + x$fn))
    s2[i] <- (x$fp / (x$fp + x$tn))
  }
  return(cbind(s2, s1))
}

confMat <- function(val) {
  mmdat <- matrix(c(val$tp, val$fn, val$fp, val$tn), 
                  nrow = 2, ncol=2, byrow = FALSE,
                  dimnames = list(c("Actual True", "Actual False"),
                                  c("Predicted True", "Predicted False")))
  return(mmdat)
}

measures <- function(val) {
  tp <- val$tp
  tn <- val$tn
  fn <- val$fn
  fp <- val$fp
  
  sensitivity <- tp / (tp + fn) # tp rate, recall, hit rate
  specificity <- tn / (tn + fp) # tn rate
  
  precision <- tp / (tp + fp) # ppv
  recall <- tp / (tp + fn)
  
  npv <- tn / (tn + fn) #negative predictive value
  fpv <- fp / (fp + tp) #false positive rate
  fallout <- fp / (fp + tn) # also fp rate
  
  accuracy <- (tp + tn) / (tp + fp + fn + tn)
  
  mcc <- tp * tn + fn * fp /
    sqrt((tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))
  
  f1 <- 2 * precision * recall / (precision + recall)
  ret <- list(sensitivity = sensitivity,
              specificity = specificity,
              precision   = precision,
              recall      = recall,
              fallout     = fallout,
              npv         = npv,
              fpv         = fpv,
              accuracy    = accuracy,
              mcc         = mcc,
              f1          = f1)
  return(ret)
}

# filter <- function(dataSet, field, stddev) {
#   x <- mean(dataSet[,field]) + stddev * sd(dataSet[,field])
#   dataSet[dataSet[,field] > x,]
# }

