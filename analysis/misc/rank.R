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
j <- 1
dtot <- c(0, 0, 0, 0, 0, 0, 0)
for (i in c(1:len))
{
  tmp <- data[[i]][j,]
  vect <- c(tmp[[1]], tmp[[2]], tmp[[3]], tmp[[4]], tmp[[5]], tmp[[6]], tmp[[7]])
  ord <- order(vect)
  d <- c(0,0,0,0,0,0,0)
  for (k in 1:7)
  {
    d[k] <- order(ord)[k]
  }
  dtot <- dtot + d  

}
order(order(rank.min[6,], decreasing = TRUE))
################################################################


