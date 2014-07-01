library(ReadImages)
img <- read.jpeg( 'data/empire.jpg')
plot(img)
i <- img[,,3]
img.red <- t(apply(img[,,1], 2, rev))
img.green <- t(apply(img[,,2], 2, rev))
img.blue <- t(apply(img[,,3], 2, rev))


image(img.red, col=rgb((0:15)/15, green=0, blue=0))
image(img.green, col=rgb(red=0, (0:15)/15, blue=0))
image(img.blue, col=rgb(red=0, (0:15)/15, green=0))


x1 <- img
x1[,,1] <- 0
x1[,,2] <- 0
x2 <- img
x2[,,1] <- 0
x2[,,3] <- 0
x3 <- img
x3[,,2] <- 0
x3[,,3] <- 0
plot(x1)
plot(x2)
plot(x3)
greyImg <- rgb2grey(img)

num <- dim(greyImg)[1]*dim(greyImg)[2]
val <- matrix(rnorm(num, 0, .25), dim(greyImg))
img2 <- greyImg + val
img2[img2 > 1] <- 1
img2[img2 < 0] <- 0
plot(img2)
imagematrix(val)

library(MASS)
vv <- mvrnorm(num, mu=c(0, 0), Sigma=matrix(c(.2,0,0,.2),2,2))
xx <- convolve(greyImg, vv)
