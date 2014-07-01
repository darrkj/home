# Read topics file
topics <- read.delim("doc_topics.txt", header = FALSE)

topics <- topics[2:938, ]
topics <- topics[, 1:22]

for(i in topics[, 1]) {
  cat(paste('<node id="p', i, '" start="', 
            floor(runif(1, min = 1, max = 72844)),
            '" end="72844" >\n</node>', sep = ''))
}

sink('xx.gexf', append = FALSE)

cat('<gexf xmlns="http://www.gexf.net/1.1draft"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://www.gexf.net/1.1draft
                             http://www.gexf.net/1.1draft/gexf.xsd"
version="1.1">
  <graph mode="dynamic" defaultedgetype="directed">
  <attributes class="node" mode="dynamic">
  <attribute id="price" title="Price" type="float"/>
  </attributes>
  <nodes>')


top <- 1:15
pap <- 1:936
time <- 10000

x1 <- function(x) {
  if(x == 0) c(1:3, sample(1:15)[1]) 
  else if (x == 1) c(1:3, sample(1:15)[1])
  else if(x == 2) c(4:7, sample(1:15)[1])
  else if(x == 3) c(8:10, sample(1:15)[1])
  else if(x == 4) c(11:13, sample(1:15)[1])
  else c(14:15, sample(14:15)[1])
}

x <- rep(NA, length(top) + length(pap))
for (i in top) {
  x[i] <- paste('v', i, sep = '')
}

for (i in pap) {
  x[i+length(top)] <- paste('p', i, sep = '')
}


y1 <- as.data.frame(x = rep(1, length(top)))
names(y1) <- 'Start'

y2 <- as.data.frame(x = floor(runif(length(pap), min = 1, max = time)))
names(y2) <- 'Start'

y <- rbind(y1, y2)

tpMod <- cbind(x, y)
tpMod$Finish <- time


node <- function(name, start, finsish) {
  cat(paste('<node id="', name, '" label="', name,
            '" start="', start,
            '" end="', finsish,'">\n</node>\n', sep = ''))
}


edge <- function(sourc, target, start) {
  cat(paste('    <edge source ="', sourc, '" target="v', 
            target, '" start="', start, '"/>\n', sep = ''))
}

for (i in 1:nrow(tpMod)) {
  node(tpMod[i, 1], tpMod[i, 2], tpMod[i, 3])
}

cat("</nodes>\n<edges>\n")


xx <- 0
for (i in (max(top)+1):nrow(tpMod)) {
  rOrd <- unique(x1(xx))
  for( j in rOrd) {
    edge(tpMod[i, 1], j, tpMod[i, 2])
  }
  xx <- xx + 1
  if (xx == 6) xx <- 0
}
  
cat("    </edges>\n  </graph>\n</gexf>") 

sink()
  


  
  
  
  