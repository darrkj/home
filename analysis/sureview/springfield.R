xx <- data.frame(id = rep(1:10, each = 5),
                 trig = rep(c('a', 'b', 'c', 'd', 'e'), 5),
                 cnt = rpois(50, 10))


uid <- data.frame(id = unique(xx$id))

for (i in unique(xx$trig)[1:3]) {
  tmp <- xx[xx$trig == i, -2]
  names(tmp)[2] <- i
  uid <- merge(uid, tmp, all.x = T)

}


reshape(data = xx, idvar = 'id', direction = 'wide', timevar = 'trig')
