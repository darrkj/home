# http://en.wikipedia.org/wiki/Aeon_(Thelema)

c <- (1:500)/20

ts.Data <- zoo(sin(c)+runif(500), as.Date("2013-04-20") + c)
plot(ts.Data)


s1 <- sample(x = ts.Data, size = 250)
s2 <- sample(x = ts.Data, size = 250)

plot(s1)
plot(s2)

library(dtw)

dtw(s1, s2)

## Find the best match
align <- dtw(s1, s2)

## Display the mapping, AKA warping function - may be multiple-valued
## Equivalent to: plot(alignment,type="alignment")
plot(align$index1, align$index2, main = "Warping function");

## Confirm: 25 samples off-diagonal alignment
lines(1:100-25, col = "red")







## A noisy sine wave as query
idx<-seq(0,6.28,len=100);
query<-sin(idx)+runif(100)/10;

## A cosine is for reference; sin and cos are offset by 25 samples
reference<-cos(idx)
plot(reference); lines(query,col="blue");

## Find the best match
alignment<-dtw(query,reference);

## Display the mapping, AKA warping function - may be multiple-valued
## Equivalent to: plot(alignment,type="alignment")
plot(alignment$index1,alignment$index2,main="Warping function");

## Confirm: 25 samples off-diagonal alignment
lines(1:100-25,col="red")

