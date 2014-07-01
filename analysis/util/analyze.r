##################################################################
#
#                        Init
#
##################################################################
library(boot)
library(stats)
#Create text file to output data into
#sink("anal.txt", append=FALSE, split=FALSE)

#load adult training data set with missing values
train <- read.table("adult-train.csv", header=T, sep=",")

#load sdult test data set with missing values
test <- read.table("adult-test.csv", header=T, sep=",")
##################################################################
#
#            Exploritory Data Analysis
#
##################################################################
#create list for output
name <- list()             #name of varaible
type <- list()             #whether the variable is cont or cat
num.mv <- list()           #number of missing value for varible 
means <- list()            #mean for cont varaible
attrib <- list()           #num of each attribute type for cat var
classes <- list()          #num of classes for cat var
mean.cond <- list()        #mean conditional on outcome
attrib.t <- list()         #num of types of attrib cond on output
attrib.f <- list()         #same as above for outcome
mv.true <- list()
mv.false  <- list()

instances <- dim(train)[1]         #number of instances in data set
vars <- dim(train)[2] - 1          #number of pred vars in data set
vars.names <- colnames(train)      #names of vars
for (i in c(1:vars))
{
	name <- c(name, c=vars.names[i])           #add name to list
	num.mv <- c(num.mv, c=sum(is.na(train[[i]]))) #num of miss values
	#only for catagorical vars
	if (is.factor(train[[i]]))
	{
		type <- c(type, "catagorical")
		#nuber of each type of occurance
		attrib <- c(attrib, list(table(train[[i]])))
		#conditional number of each type oc occurance
		attrib.t <- c(attrib.t, list(tapply(train[[i]], train$y, table)[1]))
		attrib.f <- c(attrib.f, list(tapply(train[[i]], train$y, table)[2]))
		#number of classes
		mv.true <- c(mv.true, list(tapply(train[[i]], train$y, is.na)[1]))
		mv.false <- c(mv.false, list(tapply(train[[i]], train$y, is.na)[2]))
		classes <- c(classes, dim(attrib[[i]]))
		#only for cont vars but to keep index the same
		means <- c(means, NA)
		mean.cond <- c(mean.cond, NA)
  }
  #only for continous vars
  if (is.numeric(train[[i]]))
  {
		type <- c(type, "continious")
		means <- c(means, mean(train[i], na.rm=TRUE))
		mean.cond <- c(mean.cond, list(with(train, tapply(train[[i]], y, mean, na.rm=TRUE))))
		mv.true <- c(mv.true, list(tapply(train[[i]], train$y, is.na)[1]))
		mv.false <- c(mv.false, list(tapply(train[[i]], train$y, is.na)[2]))
		attrib <- c(attrib, NA)
		attrib.t <- c(attrib.t, NA)
		attrib.f <- c(attrib.f, NA)
		classes <- c(classes, NA)
  }
}
num.mv <- as.numeric(num.mv)
prop <- table(train$y)
#number of zeros divided by number of ones  
ratio <- prop[[1]]/prop[[2]]
trues <- prop[[2]]/instances
falses <- prop[[1]]/instances
#####################################
sample <- floor(dim(train)[1]*0.01); 

bds <- sample(rownames(train),sample)

samp <- train[bds,c(1,3,5)]
pairs(samp)
zz <- cor(samp, use="pairwise.complete.obs")

zz <- cor(samp, use="pairwise.complete.obs", method="kendall") 
#################################################
write.table(t(attrib[[2]]), file = "means.csv", append = FALSE, quote = FALSE, 
	sep = ", ", eol = "\n", na = "NA", dec = ".", row.names = FALSE, 
	col.names = FALSE, qmethod = c("escape", "double"))

vars <- dim(train)[2] - 1

barplot(table(train[2]))
barplot(table(train[10]))
hist(train[[1]], freq=FALSE)
lines(density(train[[1]]), col="red")
hist(train$age[train$y == 1], freq=FALSE)
hist(train$age[train$y == 0], freq=FALSE)

hist(train[[5]], freq=FALSE)
lines(density(train[[5]]), col="red")
hist(train$education.num[train$y == 1], freq=FALSE)
hist(train$education.num[train$y == 0], freq=FALSE)

histogram(~ y | sex, data=train, type="count")#, breaks=c(3.5:10.5), col="red")
