# Useful function for get an average time.
timer <- function(fun, iters, ...) {
  avgTime <- 0
  for (n in seq(iters)) {
    avgTime <- avgTime + system.time(fun(...))[3]
  }
  return(avgTime / iters)
}

# Less verbose but captures overhea of for loop.
timer2 <- function(fun, iters, ...) {
  system.time(for(n in seq(iters)) fun(...))[3] / iters
}

# Find missing values for each column
whichMiss <- function(data) 
{
  index   <- NULL
  count   <- NULL
  percent <- NULL
  name    <- NULL
  y <- nrow(data)
  for (i in 1:length(data))
  {
    x <- sum(is.na(data[,i]))
    if (x > 0) 
    {
      index   <- c(index,   i)
      count   <- c(count,   x)
      percent <- c(percent, x/y)
      name    <- c(name,    names(data)[i])
    }
  }
  miss <- list(index=index, count=count, percent=percent, name=name)
}

#####################################################################

# wmae1 <- function() 
# {
#   for(i in 1:nrow(observed))
#   {
#     store[i]   }
#     return(sum(store)*(1/sum(weight)))
#   }
# }

# a is a vector of observed scores
# p is a vector of predictions
# w is a vector of weights
wmae2 <- function(a, p, w) {
  return(sum(abs(a-p)*w)/sum(w))
}