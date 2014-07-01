# real data, last two are forced outliers
real <- data.frame(x1 = c(rnorm(98, 20, sd = 4)),
                   x2 = c(rnorm(98, 17, sd = 2)))


bounds <- c(min(real$x1), max(real$x1), min(real$x2), max(real$x2))

bnd <- 35

xx <- matrix(0, nrow = bnd, ncol = bnd)

for( i in 1:bnd) {
  for (j in 1:bnd) {
    tmp <- rbind(real, c(i, j))
    xx[i, j] <- mean(sapply(1:20, function(x) tail(ccade(tmp)$prob, 1)))
  }
  print(i)
}

heatmap(xx)
plot(real, xlim = c(0, 35), ylim = c(0, 35))
cade(real)

rowMeans(sapply(1:20, function(x) cade(real)$prob))

data(iris)
cade(iris)

data(mtcars)
cade(mtcars)




library(profr)

# real data, last two are forced outliers
real <- data.frame(x1 = c(rnorm(98, 20, sd = 4)),
                   x2 = c(rnorm(98, 17, sd = 2)))


bounds <- c(min(real$x1), max(real$x1), min(real$x2), max(real$x2))

bnd <- 35
it <- 10

xx <- matrix(0, nrow = bnd, ncol = bnd)


for( i in 1:bnd) {
  for (j in 1:bnd) {
    tmp <- rbind(real, c(i, j))
    xx[i, j] <- mean(sapply(1:it, function(x) cade(tmp, numTree = 200)$prob[nrow(tmp)]))
  }
  print(i)
}
image(xx)



# real data, last two are forced outliers
real <- data.frame(x0 = 1:200,
                   x1 = c(rnorm(98, 100, sd = 5), rnorm(98, 50, sd = 5), c(81), c(30), c(runif(2, 0, 120))),
                   x2 = c(rnorm(98, 100, sd = 10), rnorm(98, 50, sd = 5), c(21), c(100), runif(2, 0, 120)))

plot(real)

cade(real)

rowMeans(sapply(1:20, function(x) cade(real)$prob))

data(iris)
cade(iris)

data(mtcars)
cade(mtcars)


# Removes uniform dist added values (red dots)
rxx <- xx[1:nrow(real)]

head(rev(sort(rxx)), 4)

# should contain 201 and 202
tail(order(rxx), 4)



##############


