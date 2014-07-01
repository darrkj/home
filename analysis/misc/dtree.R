# Load data set
car <- read.csv("car.csv", sep = ',', header=TRUE)
trans <- read.csv("transfusion.csv", sep=',', header=TRUE)

dep <- dim(car)[2]
atts <- dep - 1
size <- dim(car)[1]

entropy <- function(y) {
  p1 <- length(car[y == 1,])/size
  p0 <- length(car[y == 0,])/size
  ent <- - (p1 * log2(p1) + p0 * log2(p0)) 
}


gain <- function(tab) {
  gInf <- NULL
  if (is.factor(tab[,1])) {
      types <- length(levels(tab[,1]))
      len <- dim(tab)[1]
      for (j in 1:types) {
        tt <- dim(tab[tab[,1]==levels(tab[,1])[j] & tab[,2]==0,])[1]
        ff <- dim(tab[tab[,1]==levels(tab[,1])[j] & tab[,2]==1,])[1]
        tmp <- - (tt/len) * log2(tt/len) - (ff/len) * log2(ff/len)
        gInf <- c(gInf, tmp)
      }
  }
  if (is.numeric(tab[,1])) {
    vals <- summary(trans[,1])
    vals <- vals[c(2,4,5)]
    for (j in 1:3) {
        tt <- dim(tab[tab[,1]<=vals[j] & tab[,2]==0,])[1]
        ff <- dim(tab[tab[,1]>=vals[j] & tab[,2]==1,])[1]
        tmp <- - (tt/len) * log2(tt/len) - (ff/len) * log2(ff/len)
        gInf <- c(gInf, tmp)
      }
  }
  return(gInf)
}
   
x <- c(NULL)
for (i in 1:atts) {
  table <- data.frame(car[,i], car[,dep])
  x <- c(x, gain(table))
}


