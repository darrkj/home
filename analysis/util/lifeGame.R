###############################################################################
#
#     numAdj
#
###############################################################################
#' Calculate the number of live neighbers.
#' 
#' In life simulation how many cells are alive around given cell.
#' 
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @return side effect, plot
#' @param grid Game of life grid containing state
#' @param x The x cell
#' @param y The y cell
#' @keywords simulation cellular automata
#' @examples
#' # Add example
#' 

# TODO: life: turn into demo
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


###############################################################################
#
#     Alive
#
###############################################################################
#' State in next iteration
#' 
#' In life simulation a function to determine the state at the next
#' iteration of the simulation.
#' 
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @return Boolean denoting if the cell with survive this iteration
#' @param grid Game of life grid containing state
#' @param cur The current status if the cell under consideration
#' @param n The number of alive cells around given cell
#' @keywords simulation cellular automata
#' @examples
#' # Add example
#' 

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

###############################################################################
#
#     update
#
###############################################################################
#' Update the state 
#' 
#' A function which will take the current state of the simulation and 
#' determine what the state will be in the next iteration.
#' 
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @return The state in teh next iteration
#' @param grid Game of life grid containing state
#' @keywords simulation cellular automata
#' @examples
#' # Add example
#' 
update <- function(grid) {
  x <- nrow(grid)
  y <- ncol(grid)
  new <- matrix(nrow = x, ncol = y)
  for (j in 1:x) {
    for (i in 1:y) {
      tmp <- numAdj(grid, j, i)
      new[j, i] <- alive(grid[j, i], tmp)
    }
  }
  return(new)
}

###############################################################################
#
#     runGame
#
###############################################################################
#' Simulation controller
#' 
#' A function which will run a game of life simulation.
#' 
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @return side effect, plots
#' @param size The grid size for the simulation
#' @param its The number of iterations for which teh simulation should run.
#' @keywords simulation cellular automata
#' @export
#' @examples
#' runGame(20, 20)
#' 

runGame <- function(size, its) {
  state <- matrix(rbinom(size^2, 1, .5), nrow = size, ncol = size)
  for (i in seq(its)) {
    state <- update(state)
    image(t(abs(state - 1)))
  }
}

