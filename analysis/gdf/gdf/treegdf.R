library(rpart)

x1 <- runif(100, 0, 1)
x2 <- runif(100, 0, 1)
y <- NULL
for (i in 1:100) {
    v1 <- x1[i]
    v2 <- x2[i]
  if (v1 > 0.6) {
    if (v2 < 0.8) {
      v3 <- 2
    } else if (v2 >= 0.8) {
      v3 <- 2.5
    }
  } else if (v1 <= 0.6) {
    if (v2 < 0.3) {
      v3 <- 0.3
    } else if (v2 >= 0.3) {
      if (v1 < 0.3) {
        v3 <- -2
      } else if (v1 >= 0.3) {
        v3 <- -1
      }
    }
  }
  y[i] <- v3
}
#y <- y + rnorm(100, 0, .5)
ranData <- data.frame(x1, x2, y)
treemod <- rpart(y ~ x1 + x2, data = ranData)
yhat <- predict(treemod, ranData)

ye <- NULL
yehat <- NULL
diff <- NULL
for (j in 1:100) {
  ranData$ye <- ranData$y + rnorm(100, 0, 1)
  ye <- cbind(ye, ranData$ye)
  treemode <- tree(ye ~ x1 + x2, data = ranData)
  xx <- snip.tree(treemode, 2)
  yehat <- cbind(yehat, predict(xx, ranData))
  diff <- cbind(diff, predict(xx, ranData))
  delyhat <- yehat - yhat
  dely <- ranData$ye - ranData$y
  #diff <- c(diff, lm(delyhat ~ dely)[[1]][2])
}

sums <- NULL
for (j in 1:100) {
  sums <- rbind(sums, lm(yehat[,j] ~ ye[,j])[[1]][[2]])
}
mean(sums) * 4



