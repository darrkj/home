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

#data2 <- read.csv("waveform.csv")
#data3 <- data2


treeModelTrainer <- function(data, K) {
  library(rpart)
  tree <- rpart(y ~ . , data = data, control = rpart.control(minsplit = 2, cp = 0.0001))
  
  i.alpha <- which.min(abs(tree$cptable[,"nsplit"] - K))
  alpha.K <- tree$cptable[i.alpha, "CP"]
  tree.p <- prune(tree, cp = alpha.K)
  return(tree.p)
}

gdfRange <- function(data, model, range) {
  kll <- rep(NA, length(range))
  for (j in range) {
    ll <- NULL
    for (i in 1:10) {
      ll <- c(ll, GDF(data, modelTrainer = model, k = j, reps = 50, sd = .25))
    }
    kll[j] <- mean(ll)
  }
  plot(kll)
  kll
}

 
gdfRange(gen2dData(100, 0.5), treeModelTrainer, 1:10)
#gdfRange(data2, treeModelTrainer, 1:10)