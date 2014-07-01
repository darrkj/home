merge_sort <- function(m) {
  if (length(m) <= 1 )
    return(m)
  left <-  NULL
  right <- NULL
  middle <- floor(length(m) / 2)
  for (i in 1:(middle)) 
    left <- c(left, m[i])
  for (i in (middle+1):length(m))
    right <- c(right, m[i])
  left <- merge_sort(left)
  right <- merge_sort(right)
  return(merge(left, right))
}

merge <- function(left, right) {
  result <- NULL
  while(length(left) > 0 | length(right) > 0) {
    if (length(left) > 0 & length(right) > 0) {
      if (left[1] <= right[1]) {
        result <- c(result, left[1])
        left <- tail(left, -1)
      } else {
        result <- c(result, right[1])
        right <- tail(right, -1)
      }
    } else if (length(left) > 0) {
      result <- c(result, left[1])
      left <- tail(left, -1)
    } else if (length(right) > 0) {
      result <- c(result, right[1])
      right <- tail(right, -1)
    }
  }
  return(result)
}


#######################################################

merge_sort2 <- function(m) {
  if (length(m) <= 1 )
    return(m)
  left <-  NULL
  right <- NULL
  middle <- floor(length(m) / 2)
  for (i in 1:(middle)) 
    left <- c(left, m[i])
  for (i in (middle+1):length(m))
    right <- c(right, m[i])
  left <- merge_sort2(left)
  right <- merge_sort2(right)
  return(merge2(left, right))
}

merge2 <- function(left, right) {
  result <- NULL
  while(length(left) > 0 | length(right) > 0) {
    if (length(left) > 0 & length(right) > 0) {
      if (left[1] <= right[1]) {
        result <- c(result, left[1])
        left <- left[-1]
      } else {
        result <- c(result, right[1])
        right <- right[-1]
      }
    } else if (length(left) > 0) {
      result <- c(result, left[1])
      left <- left[-1]
    } else if (length(right) > 0) {
      result <- c(result, right[1])
      right <- right[-1]
    }
  }
  return(result)
}


############################################
#######################################################

merge_sort3 <- function(m) {
  if (length(m) <= 1 )
    return(m)
  left <-  NULL
  right <- NULL
  middle <- floor(length(m) / 2)
  for (i in 1:(middle)) 
    left <- c(left, m[i])
  for (i in (middle+1):length(m))
    right <- c(right, m[i])
  left <- merge_sort3(left)
  right <- merge_sort3(right)
  return(merge3(left, right))
}

merge3 <- function(left, right) {
  result <- rep(NA, (length(left) + length(right)))
  i <- 1
  while(length(left) > 0 | length(right) > 0) {
    if (length(left) > 0 & length(right) > 0) {
      if (left[1] <= right[1]) {
        result[i] <- left[1]
        i <- i + 1
        left <- left[-1]
      } else {
        result[i] <- right[1]
        i <- i + 1
        right <- right[-1]
      }
    } else if (length(left) > 0) {
      result[i] <- left[1]
      i <- i + 1
      left <- left[-1]
    } else if (length(right) > 0) {
      result[i] <- right[1]
      i <- i + 1
      right <- right[-1]
    }
  }
  return(result)
}

##########################################################

merge_sort4 <- function(m) {
  if (length(m) <= 1 )
    return(m)
  left <-  NULL
  right <- NULL
  middle <- floor(length(m) / 2)
  for (i in 1:(middle)) 
    left <- c(left, m[i])
  for (i in (middle+1):length(m))
    right <- c(right, m[i])
  left <- merge_sort4(left)
  right <- merge_sort4(right)
  return(ms4(left, right))
}

merge4 <- function(left, right) {
  result <- rep(NA, (length(left) + length(right)))
  i <- 1
  while(length(left) > 0 | length(right) > 0) {
    if (length(left) > 0 & length(right) > 0) {
      if (left[1] <= right[1]) {
        result[i] <- left[1]
        i <- i + 1
        left <- left[-1]
      } else {
        result[i] <- right[1]
        i <- i + 1
        right <- right[-1]
      }
    } else if (length(left) > 0) {
      result[i] <- left[1]
      i <- i + 1
      left <- left[-1]
    } else if (length(right) > 0) {
      result[i] <- right[1]
      i <- i + 1
      right <- right[-1]
    }
  }
  return(result)
}
ms4 <- cmpfun(merge4)

merge_sort5 <- cmpfun(merge_sort4)

test <- sample(22000)

# Call the R code profiler and give it an output file to hold results

Rprof("merge1.out")
y = merge_sort(test)
Rprof(NULL)
summaryRprof('merge1.out')[c(2,4)]

Rprof("merge2.out")
y = merge_sort2(test)
Rprof(NULL)
summaryRprof('merge2.out')[c(2,4)]

Rprof("merge3.out")
y = merge_sort3(test)
Rprof(NULL)
summaryRprof('merge3.out')[c(2,4)]

Rprof("merge4.out")
y = merge_sort4(test)
Rprof(NULL)
summaryRprof('merge4.out')[c(2,4)]

Rprof("merge5.out")
y = merge_sort5(test)
Rprof(NULL)
summaryRprof('merge5.out')[c(2,4)]



x1 <- summaryRprof('merge2.out')[[2]][,1]
x2 <- summaryRprof('merge3.out')[[2]][,1]

x1 - x2

library(profr)
p <- profr(merge_sort(test))
plot(p)
q <- profr(merge_sort2(test))
plot(q)
r <- profr(merge_sort3(test))
plot(r)

library(compiler)

ms2 <- cmpfun(merge_sort2)
ms3 <- cmpfun(merge_sort3)

Rprof("merge.out")
y = sort(sample(19450000))
Rprof(NULL)
summaryRprof('merge.out')[c(2,4)]
system.time(sort(sample(19450000)))

t <- NULL
n <- NULL
for (i in seq(10)) {
  scale <- i * 500 
  t <- append(t, system.time(merge_sort2(sample(scale)))[3])
  n <- append(n, (scale))
}
plot(n, t)
