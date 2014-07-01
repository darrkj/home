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


spin.plot <- function(data, k=45, path="~/tmp/") {
  tx <- seq(0,360,k)
  par(pty="s")
  for (i in 1:length(tx)) {
    t0 <- pi*tx[i]/180
    znew <- data[,3]
    ynew <- data[,1]*cos(t0)+data[,2]*sin(t0)
    xnew <- -data[,1]*sin(t0)+data[,2]*cos(t0)
    temp <- paste("rotation in degrees =",tx[i])
    png(paste(path,"fig",i,".png",sep=""))
    plot(ynew,znew,pch=19,cex=.8,xlab="",ylab="",
         xlim=c(-4,4),ylim=c(-4,4),main=temp)
    lines(c(0,0),c(0,2),col="red")
    lines(c(0,2*cos(t0)),c(0,0),col="red")
    lines(c(0,2*cos(t0+pi/2)),c(0,0),col="red")
    text(0,2,"Z",col="red")
    text(2*cos(t0),-0.3,"Y",col="red")
    text(2*cos(t0+pi/2),0.3,"X",col="red")
    dev.off()
  }
}



#source(url("http://aliquote.org/pub/spin_plot.R"))
dd <- replicate(3, rnorm(100))
spin.plot(dd)

gol <- function(N=NA,n=20,m=20,maxs=100) {
  require(simecol)
  if(!is.matrix(N)) { 
    N <- sample(c(0,1),n*m,replace=T) 
    dim(N) <- c(n,m) 
  }
  steps <- 0
  image(N,col=c("grey","darkgreen"),axes=F)  
  while(steps<maxs) {
    steps <- steps+1
    B <- eightneighbours(N)
    N[which(N==1& B!=2 & B!=3)] <- 0
    N[which(N==0&B==3)] <- 1
    image(N,col=c("grey","darkgreen"),axes=F,add=T)	
    Sys.sleep(0.15)
  }
}

#Can be called without any argument specified:
  gol()

