# Game of Life

# Calculate the number of live neighbers.

numAdj <- function(grid, x, y) {
  xmax <- nrow(grid)
  ymax <- ncol(grid)
  sx <- (x - 1):(x + 1)
  sy <- (y - 1):(y + 1)
  if (x == 1) {
    sx <- 1:2
  } else if (x == xmax) {
    sx <- (x - 1):x
  }
  if (y == 1) {
    sy <- 1:2
  } else if (y == ymax) {
    sy <- (y - 1):y
  }
  n <- sum(grid[sx, sy]) - grid[x, y]
  return(n)
}

# What will happen to a cell next iteration
alive <- function(cur, n){
  if (cur & n < 2) {
    #Any live cell with fewer than two live neighbours dies, as if caused by under-population.
    x <- 0
  } else if (cur & (n == 2 | n == 3)) {
    #Any live cell with two or three live neighbours lives on to the next generation.
    x <- 1
  } else if (cur & n > 3) {
    #Any live cell with more than three live neighbours dies, as if by overcrowding.
    x <- 0
  } else if (!cur & n == 3) {
    #Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
    x <- 1
  } else {
    x <- 0
  }
  return(x)
}


update <- function(grid) {
  j <- nrow(grid)
  k <- ncol(grid)
  new <- matrix(nrow=j, ncol=k)
  for (x in 1:j) {
    for (y in 1:k) {
      tmp <- numAdj(grid, x, y)
      new[x, y] <- alive(grid[x, y], tmp)
    }
  }
  return(new)
}


runGame <- function(size, its) {
  x <- size
  y <- x * x
  xx <- matrix(rbinom(y, 1, .5), nrow = x, ncol = x)
  for (i in seq(its)) {
    xx <- update(xx)
    image(t(abs(xx - 1)))
  }
}


library(rbenchmark)
benchmark(runGame2(10, 20), runGame(10, 20))


Rprof("life.out")
y <- runGame(100, 200)
Rprof(NULL)
summaryRprof('life.out')[c(2, 4)]


library(profr)
x1 <- summaryRprof('life.out')


p <- profr(runGame(30, 50))
plot(p)

