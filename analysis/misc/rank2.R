load("C:/Users/HP USER/Dropbox/Final/data/output.RData")
rm(names)
ind <- order(prop)
names <- files[ind]
prop <- prop[ind]
data <- result[ind]
size <- size[ind]

rm(ind, files, result)
len <- 20

#iterate over both
j <- 61

dmax <- c(0, 0, 0, 0, 0, 0, 0)
dmin <- dmax
for (i in c(1:len))
{
  tmp <- data[[i]][j,]
  vect <- c(tmp[[1]], tmp[[2]], tmp[[3]], tmp[[4]], tmp[[5]], tmp[[6]], tmp[[7]])

  dmax <- dmax + max(vect) - vect
  dmin <- dmin +vect - min(vect)

}

################################################################


