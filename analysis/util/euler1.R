#Problem 1
accum <- 0
for ( i in 1:999) {
  if (i %% 3 == 0) {
    accum <- accum + i
  } else if (i %% 5 == 0) {
    accum <- accum + i
  }
}


#Problem 2
len = 33
fibvals = numeric(len)
fibvals[1] = 1
fibvals[2] = 1
for (i in 3:len) { 
   fibvals[i] = fibvals[i-1] + fibvals[i-2]
}
accum <- 0
for ( i in 1:len) {
  if (fibvals[i] %% 2 == 0) {
    accum <- accum + fibvals[i]
  }
}

#Problem 3
 71      839     1471     6857    59569   104441   486847  1234169  5753023 10086647
val <- 600851475143
val1 <- ceiling(600851475143/59569)
vect <- NULL
x <- 1
for (i in 1:val1) {
  if (val %% i == 0) { 
    vect[x] <- i
    x <- x + 1
  }
}

factors <- c(71, 839, 1471, 6857, 59569, 104441, 486847, 1234169, 5753023, 10086647)
x <- factors[4]
y <- "prime"
for (i in 2:(x-1)) {
  if (x %% i == 0) {
    y = "not Prime"
    break
  }
}
  
  
    
    
    
    
    
    
    
    
  