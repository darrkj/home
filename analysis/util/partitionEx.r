# example of partitioning

adult <- read.table("adult.csv", header=F, sep=",") #load data file

size <- floor(dim(adult)[1]*0.4); 

bds<-sample(rownames(adult),size)

tr_adult<-adult[bds,]
te_adult <- adult[setdiff(rownames(adult), bds),]
