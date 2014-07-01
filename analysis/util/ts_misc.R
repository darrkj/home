holidays <- c(as.Date("2013-04-20"))
dd <- as.Date("2013-04-20") + (-10:10)
ee <- as.Date("2013-04-20") + (0:20)

dd.Data <- zoo(sin(c)+runif(500), dd)
ee.Data <- zoo(sin(c)+runif(500), ee)

ts.merge <- function(ts1, ts2, fun = mean) {
  x1 <- which(time(ts1) %in% time(ts2))
  x2 <- which(time(ts2) %in% time(ts1))
  apply(cbind(ts1[x1], ts2[x2]), 1, fun)
}


ts.merge(dd.Data, ee.Data)


x <- birds[[1]]@left

plot(x)






# Partial dates

ch <- as.Date("2013-12-25")
dd <- as.Date("2013-04-20") + (-1000:10)
ee <- as.Date("2013-04-20") + (0:20)



