# german <- read.csv("german.csv", header = FALSE)
# 
# 
# cars$x <- cars$speed
# cars$speed <- NULL
# 
# cars$y <- cars$dist
# cars$dist <- NULL
# 
# mod <- lm(y ~ x, data = cars)
# cars$yhat <- predict(mod, cars)
# 
# cars$ye <- cars$y + rnorm(50, 0, 1)
# cars$dely <- cars$ye - cars$y
# 
# mod2 <- lm(ye ~ x, data = cars)
# cars$yehat <- predict(mod, cars)
# 
# cars$delyhat <- cars$yehat - cars$yhat
# 
# mod3 <- lm(yehat ~ ye, data = cars)
# 
# 
# mod3[[1]]


cars$x <- cars$speed
cars$speed <- NULL

cars$y <- cars$dist
cars$dist <- NULL

mod <- lm(y ~ x, data = cars)
cars$yhat <- predict(mod, cars)

sums <- 0
for (i in 1:100) {
  cars$ye <- cars$y + rnorm(nrow(cars), 0, 1)
  cars$dely <- cars$ye - cars$y

  mod2 <- lm(ye ~ x, data = cars)
  cars$yehat <- predict(mod, cars)

  cars$delyhat <- cars$yehat - cars$yhat

  mod3 <- lm(yehat ~ ye, data = cars)

  sums <- sums + mod3[[1]][2]
}



