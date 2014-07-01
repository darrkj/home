

library(igraph)
#library(sna)
#source("http://bioconductor.org/biocLite.R")
#biocLite("graph")
#library(graph)

x <- erdos.renyi.game(5, .15)

is.connected(erdos.renyi.game(25, .05))

nodeList <- NULL
for (k in 10:110) {
  propList <- NULL
  for (j in 1:50) {
    count <- 0
    length <- 3000
    for (i in 1:length) {
      count <- count + is.connected(erdos.renyi.game(k, j/100))
    }
    propList <- c(propList, count / length)
  }
  nodeList <- cbind(nodeList, propList)
}
persp(nodeList)