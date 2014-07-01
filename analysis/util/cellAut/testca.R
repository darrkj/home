
library(ggplot2)
library(plyr)
library(reshape2)
library(compiler)
library(rbenchmark)

cellAut <- function(rule, size=101) {
  x1 <- size * 2 + 1
  x2 <- size
  grid <- matrix(0, nrow = x1, ncol = x2, byrow = FALSE)
  set <- c(0, 1, 0)
  
  mid <- ceiling(x1 / 2)
  grid[mid - 1] <- set[1]
  grid[mid] <- set[2]
  grid[mid + 1] <- set[3]
  d <- sapply(rule,function(x){ as.integer(intToBits(x))})[1:8]
  d <- rev(d)
  a <- x1 + 1
  b <- x1
  c <- x1 - 1
  val <- mid + x1 - 2
  for (x in val:length(grid)) {
    if (all(grid[(x-a):(x-c)] == c(1, 1, 1))) {
      grid[x] <- d[1]
    } else if (all(grid[(x-a):(x-c)] == c(1, 1, 0))) {
      grid[x] <- d[2]
    } else if (all(grid[(x-a):(x-c)] == c(1, 0, 1))) {
      grid[x] <- d[3]
    } else if (all(grid[(x-a):(x-c)] == c(1, 0, 0))) {
      grid[x] <- d[4]
    } else if (all(grid[(x-a):(x-c)] == c(0, 1, 1))) {
      grid[x] <- d[5]
    } else if (all(grid[(x-a):(x-c)] == c(0, 1, 0))) {
      grid[x] <- d[6]
    } else if (all(grid[(x-a):(x-c)] == c(0, 0, 1))) {
      grid[x] <- d[7]
    } else if (all(grid[(x-a):(x-c)] == c(0, 0, 0))) {
      grid[x] <- d[8]
    } 
  }
  
  grid <- grid[,ncol(grid):1]
  image(1-grid)
}

cAut <- cmpfun(cellAut)

################################################

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

############################

cellAut3 <- function(rule, size=101) {
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
cAut3 <- cmpfun(cellAut3)

bin2dec.easy <- function(binaryvector) {
  9-(sum(2^(which(rev(binaryvector)==TRUE)-1))+1)
}

b2d <- cmpfun(bin2dec.easy)

##########################

cellAut4 <- function(rule, size=101) {
  x1 <- size * 2 + 1
  grid <- matrix(0, nrow=x1, ncol=size, byrow=FALSE)
  
  mid <- ceiling(x1/2)
  grid[mid,1] <- 1
  d <- rev(sapply(rule,function(x){ as.integer(intToBits(x))})[1:8])
  
  val <- mid+x1-2
  for (x in val:length(grid)) {
    ss <- grid[(x-(x1+1)):(x-(x1-1))]
    grid[x] <- d[bin2dec.easy(ss)]
  }
  grid <- grid[,ncol(grid):1]
  image(1-grid)
}
cAut4 <- cmpfun(cellAut4)
#############################

cellAut5 <- function(rule, size=11) {
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
cAut5 <- cmpfun(cellAut5)



s <- 100

benchmark(cellAut(110, s), cellAut2(110, s), 
          cellAut3(110, s), cellAut4(110, s), 
          cellAut5(110, s), replications=5)

benchmark(cAut(110, s), cAut2(110, s), 
          cAut3(110, s), cAut4(110, s), 
          cAut5(110, s), replications=20)

library(profr)



Rprof("ca1.out")
y = timer2(cAut, 5, 110, 150)
Rprof(NULL)
summaryRprof('ca1.out')




