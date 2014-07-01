library(compiler)

cellEval <- function(parents) {
  if (all(parents == c(1, 1, 1))) {
    child <- 1
  } else if (all(parents == c(1, 1, 0))) {
    child <- 2
  } else if (all(parents == c(1, 0, 1))) {
    child <- 3
  } else if (all(parents == c(1, 0, 0))) {
    child <- 4
  } else if (all(parents == c(0, 1, 1))) {
    child <- 5
  } else if (all(parents == c(0, 1, 0))) {
    child <- 6
  } else if (all(parents == c(0, 0, 1))) {
    child <- 7
  } else if (all(parents == c(0, 0, 0))) {
    child <- 8
  }
  return(child)
}

ccEval <- cmpfun(cellEval)


cellAut <- function(rule, size = 101) {
  x1 <- size * 2 + 1
  grid <- matrix(0, nrow = x1, ncol = size, byrow = FALSE)
  
  mid <- ceiling(x1 / 2)
  grid[mid,1] <- 1
  d <- rev(sapply(rule,function(x){ as.integer(intToBits(x))})[1:8])
  
  val <- mid + x1 - 2
  for (x in val:length(grid)) {
    grid[x] <- d[ccEval(grid[(x - (x1 + 1)):(x - (x1 - 1))])]
  }
  grid <- grid[,ncol(grid):1]
  image(1 - grid, axes = F)
}

#' Compiled cellular automata function
#'
#' @param rule The rule to simpulate.
#' @param size The size of the grid.
#' @keywords simulation, complexity
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @export
#' @references TODO: cAut: addreftobook
#' @examples
#' cAut(30, 101)
#' 
#' manipulate(cAut(Rule, Size), 
#'            Rule = slider(1, 256), 
#'            Size = slider(11, 201))

cAut <- cmpfun(cellAut)




