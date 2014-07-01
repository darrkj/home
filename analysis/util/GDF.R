###############################################################################
#
#                                   GDF.R
#
###############################################################################
#
# Author - Kenny Darrell
# email - darrell@datamininglab.com
#
# These are functions related to Generalized Degrees of Freedom.
# Most of this code was stolen from John's book on ensembles. 
#
###############################################################################

###
#  GDF Function - calculates the generalized degrees of freeedom, given a
#  dataset and a model trainer method.  Two other arguments are the number
#  of repititions(sample size for each value of k) and sd (standard deviation)
#  to add perturb by).  The return is the number of degrees of freedom.
#
##

GDF <- function(data, modelTrainer, k, reps, sd) {
  N <- nrow(data)
  #create list of random number, distribute across columns of matrix
  perturbations <- matrix(rnorm(N * reps, 0, sd), nrow = N)
  #create matrix that of real data plus noise above
  yeMat <- matrix(data$y, nrow = N, ncol = reps, byrow = FALSE) + perturbations
  #model of real data
  baseModel <- modelTrainer(data, k)
  #predicted values from real model
  yHat <- predict(baseModel, data)
  #initialize matrix for real plus noise predictions
  yeHatMat <- matrix(NA, nrow = N, ncol = reps)
  
  for (i in 1:reps) {
    #substitute ith col of noisey data for y
    data$y <- yeMat[,i]
    #train model on y plus noise
    modelPerturbed <- modelTrainer(data, k)
    #predict noisey data
    yeHatMat[,i] <- predict(modelPerturbed, data)
  }
  #init generalized degree of freedom coef's
  gdf <- c(NA, N)
  for (i in 1:N) {
    gdf[i] <- lm(yeHatMat[i,] - yHat[i] ~ perturbations[i,])$coefficients[2]
  }
  sum(gdf)
}

###
#  Simple tree model trainer function for input in DGF function.
#
##

treeModelTrainer <- function(data, K) {
  library(rpart)
  tree <- rpart(y ~ . , data = data, 
                control = rpart.control(minsplit = 2, cp = 0.0001))
  
  i.alpha <- which.min(abs(tree$cptable[,"nsplit"] - K))
  alpha.K <- tree$cptable[i.alpha, "CP"]
  tree.p <- prune(tree, cp = alpha.K)
  return(tree.p)
}

###
#  Method to construct example data used in ensemble book.
#
##

gen1dData <- function(N, sd) {
  x1 <- runif(N, 0, 1)
  y <- rep(NA, N)
  for (i in 1:N) {
    if ( x1[i] > .6 ) {
      y[i] <- rnorm(1, 2, sd)
    } else {
      y[i] <- rnorm(1, 0, sd)
    }
  }
  data.frame(x1, y)
}

###
#  Method to construct example data used in ensemble book.
#
##

gen2dData <- function(N, sd) {
  x1 <- runif(N, 0, 1)
  x2 <- runif(N, 0, 1)
  y <- rep(NA, N)
  for (i in 1:N) {
    if ( x1[i] > .6 ) {
      if (x2[i] > .8 ) {
        y[i] <- rnorm(1, 2.5, sd)
      }
      y[i] <- rnorm(1, 2, sd)
    } else {
      if (x2[i] < .3 ) {
        y[i] <- rnorm(1, 0, sd)
      } else {
        if (x1[i] > .3 ) {
          y[i] <- rnorm(1, -1, sd)
        } else {
          y[i] <- rnorm(1, -2, sd)
        }
      }
    }
  }
  data.frame(x1, x2, y)
}


###
#  Method to construct example data used in ensemble book.
#
##

gen3dData <- function(N, sd) {
  x1 <- runif(N, 0, 1)
  x2 <- runif(N, 0, 1)
  x3 <- runif(N, 0, 1)
  y <- rep(NA, N)
  for (i in 1:N) {
    if ( x1[i] > .6 ) {
      if (x2[i] > .8 ) {
        if (x3[i] > .1 ) {
          y[i] <- rnorm(1, 2.5, sd)
        } else {
          y[i] <- rnorm(1, 4.5, sd)
        }
      }
      y[i] <- rnorm(1, 2, sd)
    } else {
      if (x2[i] < .3 ) {
        if (x3[i] > .1 ) {
          y[i] <- rnorm(1, -3, sd)
        } else {
          y[i] <- rnorm(1, -1, sd)
        }
      } else {
        if (x1[i] > .3 ) {
          y[i] <- rnorm(1, -1, sd)
        } else {
          y[i] <- rnorm(1, -2, sd)
        }
      }
    }
  }
  data.frame(x1, x2, x3, y)
}


###
#  This will calculate an average value of generalized degrees of freedom
#  for each number of parameteres.  This can be made better by creating 
#  a default value, say 10 or 100, but also take an optional argument to
#  override this.  Could also return mean and other parameters, standard
#  deviation, etc.
#
##

gdfRange <- function(data, model, range, obs=20) {
  kVal <- rep(NA, length(range))
  sVal <- rep(NA, length(range))
  dFrame <- NULL
  xFrame <- NULL
  for (j in range) {
    gdfObs <- NULL
    for (i in 1:obs) {
      gdfObs <- c(gdfObs, 
                  GDF(data, modelTrainer = model, k = j, reps = 50, sd = .25))
    }
    tmp <- cbind(j, gdfObs)
    dFrame <- rbind(dFrame, tmp)
    xFrame$tmp <- gdfObs
    names(xFrame)[j] <- paste("k", j, sep="")
  }
  as.data.frame(dFrame)
}

plotGDF <- function(gdf) {

  kMean <- as.numeric(lapply(split(gdf$gdfObs, gdf$j), mean))
  kSd <- as.numeric(lapply(split(gdf$gdfObs, gdf$j), sd))
  num <- length(unique(gdf[,1]))
  mx <- max(gdf[,2]) + 10
  plot(gdf, xlab = "", ylab = "", xlim = c(1,num), ylim = c(0,mx))

  title(main="GDF Complexity", 
        xlab="# of Parameters", 
        ylab="Generalized Degrees of Freedom")
  
  par(new=T)
  x1 <- seq(1,length(kMean))
  y1 <- kMean
  plot(x1, y1, xlab = "", ylab = "", xlim = c(1,num), ylim = c(0,mx))
  s <- seq(length(kMean)-1)# one shorter than data
  segments(x1[s], y1[s], x1[s+1], y1[s+1], 'green')
  
  par(new=T)
  y2 <- kMean + kSd
  plot(x1, y2, xlab = "", ylab = "", xlim = c(1,num), ylim = c(0,mx))
  s <- seq(length(kSd)-1)# one shorter than data
  segments(x1[s], y2[s], x1[s+1], y2[s+1], 'red')
  
  par(new=T)
  y3 <- kMean - kSd
  plot(x1, y3, xlab = "", ylab = "", xlim = c(1,num), ylim = c(0,mx))
  s <- seq(length(kSd)-1)# one shorter than data
  segments(x1[s], y3[s], x1[s+1], y3[s+1], 'red')
}

# Test cases
# k <- gdfRange(gen1dData(100, 0.5), 
#               treeModelTrainer, 1:5, obs=20)
# 
# j <- gdfRange(gen2dData(100, 0.5), 
#               treeModelTrainer, 1:5, obs=20)
# 
# l <- gdfRange(gen3dData(100, 0.5), 
#               treeModelTrainer, 1:5, obs=20)
