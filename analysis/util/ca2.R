library(ggplot2)
library(plyr)
library(reshape2)
library(compiler)

cellEval <- function(parents) {
  if (all(parents == c(1, 1, 1))) {
    d <- 1
  } else if (all(parents == c(1, 1, 0))) {
    d <- 2
  } else if (all(parents == c(1, 0, 1))) {
    d <- 3
  } else if (all(parents == c(1, 0, 0))) {
    d <- 4
  } else if (all(parents == c(0, 1, 1))) {
    d <- 5
  } else if (all(parents == c(0, 1, 0))) {
    d <- 6
  } else if (all(parents == c(0, 0, 1))) {
    d <- 7
  } else if (all(parents == c(0, 0, 0))) {
    d <- 8
  }
  return(d)
}

ccEval <- cmpfun(cellEval)
# add isEven fnction, maybe from euler code
# then if size is even add one
cellAut2 <- function(rule, size=11) {
  x1 <- size * 2 + 1
  grid <- matrix(0, nrow=x1, ncol=size, byrow=FALSE)
  
  mid <- ceiling(x1/2)
  grid[mid,1] <- 1
  d <- rev(sapply(rule,function(x){ as.integer(intToBits(x))})[1:8])

  val <- mid+x1-2
  for (x in val:length(grid)) {
      grid[x] <- d[cellEval(grid[(x-(x1+1)):(x-(x1-1))])]
  }
  grid <- grid[,ncol(grid):1]
  image(1-grid)
}
cAut2 <- cmpfun(cellAut2)

cellAut3 <- function(rule, size=101) {
  x1 <- size * 2 + 1
  grid <- matrix(0, nrow=x1, ncol=size, byrow=FALSE)
  
  mid <- ceiling(x1/2)
  grid[mid,1] <- 1
  d <- rev(sapply(rule,function(x){ as.integer(intToBits(x))})[1:8])
  
  val <- mid+x1-2
  for (x in val:length(grid)) {
    grid[x] <- d[ccEval(grid[(x-(x1+1)):(x-(x1-1))])]
  }
  grid <- grid[,ncol(grid):1]
  image(1-grid)
}
cAut3 <- cmpfun(cellAut3)

bin2dec.easy <- function(binaryvector) {
  9-(sum(2^(which(rev(binaryvector)==TRUE)-1))+1)
}

b2d <- cmpfun(bin2dec.easy)

cellAut4 <- function(rule, size=101) {
  x1 <- size * 2 + 1
  grid <- matrix(0, nrow=x1, ncol=size, byrow=FALSE)
  
  mid <- ceiling(x1/2)
  grid[mid,1] <- 1
  d <- rev(sapply(rule,function(x){ as.integer(intToBits(x))})[1:8])
  
  val <- mid+x1-2
  for (x in val:length(grid)) {
    ss <- grid[(x-(x1+1)):(x-(x1-1))]
    grid[x] <- d[b2d(ss)]
  }
  grid <- grid[,ncol(grid):1]
  image(1-grid)
}
cAut4 <- cmpfun(cellAut4)