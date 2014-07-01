# A Star, A*
# Path finding algorithm

# First need a grid

# Matrix
len <- 5
wid <- 5



library(rblocks)

x <- matrix(data = runif(len * wid), 
            nrow = len, ncol = wid)

start <- c(5, 5)
end <- c(12, 12)

y <- expand.grid(x = 1:len, y = 1:wid)
y <- as.matrix(cbind(y, 0))


touches <- function(x, y, grid) {
  if (x != 1 & y != 1) {
    grid[x - 1, y - 1] <- 1
    grid[x, y - 1] <- 1
    grid[x - 1, y] <- 1
  }
  if (x < len & y < wid) {
    grid[x + 1, y + 1] <- 1
  }
  if (x < len + 1) {
    grid[x + 1, y + 1] <- 1
  }
  if (x < len & y < wid) {
    grid[x + 1, y + 1] <- 1
  }
  return(grid)
}

expand.grid((wid - 1):(wid + 1),
            (len - 1):(len + 1))[c(1:4,6:9), ]