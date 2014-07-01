#network specs: K communities, N nodes, n[1] of them have 
#label 1, etc..., p(link inside)=p.in, p(link outside)=p.out
K     <- 3
N     <- 40 
n     <- c(rep(floor(N/K),K-1),floor(N/K)+N%%K)
labls <- rep(seq(K),n)
p.in  <- .9
p.out <- .1

pairs      <- expand.grid(seq(N),seq(N))
uniq.pairs <- pairs[which(pairs[,1]<pairs[,2]),]
uniq.pairs <-lapply(apply(uniq.pairs,1,list),unlist) # I hate R for this line

#plot the block matrix
plot(NULL,xlim=c(1,N),ylim=c(1,N),asp=1,axes=F,xlab=NA,ylab=NA, main="p-matrix")
lapply( uniq.pairs, function(z) {
  colr <- ifelse(labls[z[1]]==labls[z[2]],gray(1-p.in), gray(1-p.out))
  points(z,N-z[c(2,1)],pch=15,col=colr) } )

#sample an adjacency list
adj.list   <- lapply(uniq.pairs,function(z) {
  p <- ifelse( labls[z[1]]==labls[z[2]],p.in,p.out)
  ifelse(runif(1)<p,1,0)*z})
adj.list   <- adj.list[ which(lapply(adj.list,sum)>0) ]


#plot the adjacency matrix
plot(NULL,xlim=c(1,N),ylim=c(1,N),asp=1,axes=F,xlab=NA,ylab=NA, main="adjacency matrix")
lapply( adj.list, function(z) {
  points(z,N-z[c(2,1)],pch=15) } )


#plot the network
plot(NULL,xlim=c(0,K),ylim=c(0,1),axes=F,xlab=NA,ylab=NA, main="network")
coords <- matrix(runif(2*N),ncol=2)+cbind(labls-1,rep(0,N))
lapply( adj.list, function(z) {
  x <- coords[ z,1 ]
  y <- coords[ z,2 ]
  lines(x,y,col="#55555544") } )
colrs <- rainbow(K)
points( coords, pch=15, col=colrs[ labls ])