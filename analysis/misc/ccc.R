library('foreach')
library('ggplot2')
library('animation')

# Determines how many neighboring cells around the (j,k)th cell have living organisms.
# The conditionals are used to check if we are at a boundary of the grid.
how_many_neighbors <- function(grid, j, k) {
  size <- nrow(grid)
  count <- 0
  if(j > 1) {
    count <- count + grid[j-1, k]
    if (k > 1) count <- count + grid[j-1, k-1]
    if (k < size) count <- count + grid[j-1, k+1]
  }
  if(j < size) {
    count <- count + grid[j+1,k]
    if (k > 1) count <- count + grid[j+1, k-1]
    if (k < size) count <- count + grid[j+1, k+1]
  }
  if(k > 1) count <- count + grid[j, k-1]
  if(k < size) count <- count + grid[j, k+1]
  count
}

# Creates a list of matrices, each of which is an iteration of the Game of Life.
# Arguments
# size: the edge length of the square
# prob: a vector (of length 2) that gives probability of death and life respectively for initial config
# returns a list of grids (matrices)
game_of_life <- function(size = 10, num_reps = 50, prob = c(0.5, 0.5)) {
  grid <- list()
  grid[[1]] <- replicate(size, sample(c(0,1), size, replace = TRUE, prob = prob))
  dev_null <- foreach(i = seq_len(num_reps) + 1) %do% {
    grid[[i]] <- grid[[i-1]]
    foreach(j = seq_len(size)) %:%
      foreach(k = seq_len(size)) %do% {
        
        # Apply game rules.
        num_neighbors <- how_many_neighbors(grid[[i]], j, k)
        alive <- grid[[i]][j,k] == 1
        if(alive && num_neighbors <= 1) grid[[i]][j,k] <- 0
        if(alive && num_neighbors >= 4) grid[[i]][j,k] <- 0
        if(!alive && num_neighbors == 3) grid[[i]][j,k] <- 1
      }
  }
  grid
}

# Converts the current grid (matrix) to a ggplot2 image
grid_to_ggplot <- function(grid) {
  # Permutes the matrix so that melt labels this correctly.
  grid <- grid[seq.int(nrow(grid), 1), ]
  grid <- melt(grid)
  grid$value <- factor(ifelse(grid$value, "Alive", "Dead"))
  p <- ggplot(grid, aes(x=X1, y=X2, z = value, color = value))
  p <- p + geom_tile(aes(fill = value))
  p  + scale_fill_manual(values = c("Dead" = "white", "Alive" = "black"))
}


game_grids <- game_of_life(size = 20, num_reps = 250, prob = c(0.1, 0.9))
grid_ggplot <- lapply(game_grids, grid_to_ggplot)
saveGIF(lapply(grid_ggplot, print), clean = TRUE)








shiftMatrix <- function(mx, dr, dc) {
  #Shift the matrix by dr (delta r) rows and dc columns
  #by adding e.g. dr rows of zeros and removing dr rows from the other side 
  
  nr <- nrow(mx)
  nc <- ncol(mx)
  
  #If the matrix is shifted by more than its nrow or ncol, we get a matrix of zeros 
  if (abs(dr) >= nr || abs(dc) >= nc) {
    mx <- matrix(0, nrow = nr, ncol = nc)
    return(mx)
  }
  
  #Rows:
  if (dr > 0) {
    mx <- rbind(mat.or.vec(dr, nc), mx)
    mx <- mx[1:nr,]
  } else if (dr < 0) {
    mx <- rbind(mx, mat.or.vec(-dr, nc))
    mx <- mx[(1 - dr):(nr - dr),]
  }
  
  #Columns:
  if (dc > 0) {
    mx <- cbind(mat.or.vec(nr, dc), mx)
    mx <- mx[,1:nc]
  } else if (dc < 0) {
    mx <- cbind(mx, mat.or.vec(nr, -dc))
    mx <- mx[,(1 - dc):(nc - dc)]
  }
  return(mx)
}

life_cycle <- function(mx) {
  #Move the board one generation forward
  
  mx0 <- matrix(0, nrow = nrow(mx), ncol = ncol(mx))
  
  #Produce 8 "shifted" boards and add them up
  for (n in (-1:1)) {
    for (m in (-1:1)) {
      if (n !=0 || m !=0) {
        mx0 <- mx0 + shiftMatrix(mx, n, m)
      }
    }
  }
  
  #Deaths and births
  mx[mx0 > 3 | mx0 < 2] <- 0
  mx[mx0 == 3] <- 1
  return(mx)
}

#Initialization of parameters
n_rows <- 100
n_cols <- 100
n_cells <- n_rows * n_cols
n_cycles <- 100
sleep_time <- 0.1
clrs <- c("#f0f0f0", "#2f81c1") #colors for empty squares and cells
rnd_threshold <- 0.3 # 0 - empty board; 1 - all squares are filled
set.seed(1) #Random seed

#Create a board and plot it 
board <- matrix(0, nrow = n_rows, ncol = n_cols)
board[runif(n_cells, 0, 1) < rnd_threshold] <- 1
image(board, axes = FALSE, col = clrs)

#The main cycle
for (i in (1:n_cycles)) {
  Sys.sleep(sleep_time)
  board <- life_cycle(board)
  image(board, axes = FALSE, col = clrs)
}








