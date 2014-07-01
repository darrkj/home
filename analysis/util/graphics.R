#library(igraph)

g1 <- graph( c(1,2,2,3,3,4,5,7,7,1, 6,7,2,4), directed=FALSE )
plot(g1)




names <- list(a=2, b=c(1, 3, 4), c=5, d=c(1, 2), e=c(2))

"Table 1"
x1 <- read.xls("OIFNames of Fallen1.xlsx", sheet = "Table 1",  method ="csv")
x2 <- read.xls("OIFNames of Fallen1.xlsx", sheet = "Table 2",  method ="csv")

y <- NULL
for (i in 1:58)
{
  yy <- read.xls("OIFNames of Fallen.xlsx", sheet = paste("Table", i),  method ="csv")
  y <- rbind(y, yy)
}