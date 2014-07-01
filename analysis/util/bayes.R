library(e1071)

len <- 25

vars <- c('x1', 'x2', 'x3', 'y')

mock <- 1:len
mock <- as.data.frame(mock)

j <- 2
for ( i in vars) {
  mock[, j] <- sample(c(0, 1), len, replace = T)
  j <- j + 1
}
names(mock) <- c('id', vars)

mod <- naiveBayes(y ~ x1 + x2 + x2, data = mock)
mod$tables$x1 <- matrix(data = c(.9, .1, .8, .2), ncol = 2, byrow = T)
dimnames(mod$tables$x1) <- list('x2', 'y')


RADR 
 - Add polar 360 plot to RADR, let metrics be the configurable options
 - Add search to RADR as its own pane
 - Add docgraph for some form of text for some area, maybe contract area
 - Add intercept triggers, maybe to eDW connection to show tripwires