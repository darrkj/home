#### Linear Regression

library(MASS)
library(stats)


sizelist <- c(25, 100, 200, 500, 2000, 5000) # sample size list
errlist <- c(.1, .5, 1, 2)             # noise level list
i <- length(sizelist)   # number of different samples
j <- length(errlist)   # number of different noise levels

##### Data structures x1, x2, x3 as regressors
lm_r1.array <- array(0, dim = c(i, j))
lm_ar1.array <- array(0, dim = c(i, j))
lm_mse1.array <- array(0, dim = c(i, j))
##### Data structures for stewise regression
lm_r2.array <- array(0, dim = c(i, j))
lm_ar2.array <- array(0, dim = c(i, j))
lm_mse2.array <- array(0, dim = c(i, j))
##### Data Structures for x1 and x2 as regressors
lm_r3.array <- array(0, dim = c(i, j))
lm_ar3.array <- array(0, dim = c(i, j))
lm_mse3.array <- array(0, dim = c(i, j))

k <- max(sizelist)          #This makes k is 5000
x_1 <- rnorm(k) * .5 + 2    #make x1 and x2 have length 5000
x_2 <- rnorm(k) * .1 - 1    #so later just use first n of them 

#iterate the regression over the different sample sizes and noise levels
for (n in c(1:i)) 
{
	for (m in c(1:j))  
	{
    		x1 <- x_1[1:sizelist[n]]  #take first n of of x_1 for analysis
    		x2 <- x_2[1:sizelist[n]]
    		x3 <- x1 * x2
    		e <- rnorm(sizelist[n],sd=errlist[m]) #this is the error term
    		y <- 4 + .8 * x1 + 2 * x2 + e         #observed values to regress on

   		fit <- lm(y~x1+x2+x3);  #simple linear regression with x1 x2 and x3
		summary <- summary(fit);   #relavent values from the OLS
		#pull values from summary and place into data structure
		lm_mse1.array[n, m] <- (summary$sigma)^2     #MSE
		lm_r1.array[n, m] <- summary$r.squared       #R squared
		lm_ar1.array[n, m] <- summary$adj.r.squared  #Adjusted R squared
		
		#stepwise regression on OLS fit above, in both directions
    		step <- step(fit, directions="both");  
		summary1 <- summary(step);
		lm_mse2.array[n, m] <- (summary1$sigma)^2
		lm_r2.array[n, m] <- summary1$r.squared
		lm_ar2.array[n, m] <- summary1$adj.r.squared
		
		#just a check, I think this should be better that 
		#the fit on (x1,x2,x3) and step should reduce to this
    		model <- lm(y~x1+x2);
		summary2 <- summary(model);
		lm_mse3.array[n, m] <- (summary2$sigma)^2
		lm_r3.array[n, m] <- summary2$r.squared
		lm_ar3.array[n, m] <- summary2$adj.r.squared
	}
}

#### Poisson Regression 

#Data structures as above for looping over over sample
#they are a list instead of an array as there is no 
#noise levels to iterate over
pois_dev1.array <- array(0, dim = c(i)) 
pois_aic1.array <- array(0, dim = c(i))
pois_dev2.array <- array(0, dim = c(i))
pois_aic2.array <- array(0, dim = c(i))
pois_dev3.array <- array(0, dim = c(i))
pois_aic3.array <- array(0, dim = c(i))


x_1 <- rnorm(k) * 3 + 1 #create x~Norm(1, 3^2) sample of 5000
x_2 <- rnorm(k) * .1    #create x~Norm(0, .1^2)
a <- c(0, 1)            #create 1 or o field
x_3 <- sample(a, k, replace = T) #sample the 1/0 field with replacement 
x_4 <- x_1 * x_1

for (n in c(1:i)) 
{
	x1 <- x_1[1:sizelist[n]]  #take first n as sample size of above list
   	x2 <- x_2[1:sizelist[n]]
   	x3 <- x_3[1:sizelist[n]]
   	x4 <- x_4[1:sizelist[n]]

	#the λ from log(λ)= 0.2+ 3x1 +1*x2+0.5*x3
	lamda <- exp(0.2 + 3 * x1 + x2 + 0.5 * x3)
	#construct y~Poisson(λ) with length of current sample size
	y <- rpois(n = sizelist[n], lamda)

	#GLM with Poisson Regression with (x1 x2 x3)
	poiss <- glm(y~x1+x2+x3,family=poisson); 
	summary <- summary(poiss)     #relavent values from the poisson regression
	#pull values from summary and place into data structure
	pois_dev1.array[n] <- summary$deviance   #Deviance
	pois_aic1.array[n] <- summary$aic        #AIC

	#Same as above but adding x4 and using stepwise selection
	pois2 <- glm(y~x1+x2+x3+x4,family=poisson); 
	summary2 <- summary(pois2) 
	pois_dev2.array[n] <- summary2$deviance
	pois_aic2.array[n] <- summary2$aic

	#create new lamda function
	lamda <- exp(0.2 + x1 + 3 * x2 + 0.5 * x3)
	y <- rpois(n = sizelist[n], lamda)
	#Regression on new y function
	pois3 <- glm(y~x1+x2+x3,family=poisson); 
	summary3 <- summary(pois3) 
	pois_dev3.array[n] <- summary3$deviance
	pois_aic3.array[n] <- summary3$aic

}

#########################
#Common for c and d
#########################

k <- 500
x1 <- runif(k, min=0, max=1)
x2 <- NULL

for(i in 1:k)
{
	if ((i/2-floor(i/2) > 0)) 
		x2[i] <- 0
	else
		x2[i] <- 1
}

#### Logistic Regression
logitp <-  -1 + 5.5 * x1 -0.3 * x2
p <- plogis(logitp)

y <- NULL

for(i in 1:k)
{
	y[i] <- rbinom(1, 1, p[i])
}

glm.logit <- glm(y ~ x1 + x2, family = binomial(link=logit))
glm.probit <- glm(y ~ x1 + x2, family = binomial(link=probit))

p1clog <- summary(glm.logit)
p1cprob <- summary(glm.probit)

plot(glm.logit$linear.predictors, glm.logit$y)
points(glm.logit$linear.predictors, glm.logit$fitted.values)

plot(glm.probit$linear.predictors, glm.probit$y)
points(glm.probit$linear.predictors, glm.probit$fitted.values)


####Probit Regression


probitp <-  -1 + 5.5 * x1 - 0.3 * x2
p <- pnorm(probitp)

y <- NULL;

for(i in 1:k)
{
	y[i] <- rbinom(1, 1, p[i])

}

glm.logit1 <- glm(y ~ x1 + x2, family = binomial(link="logit"))
glm.probit1 <- glm(y ~ x1 + x2, family = binomial(link="probit"))


p1dlog <- summary(glm.logit1)
p1dprob <- summary(glm.probit1)

plot(glm.logit1$linear.predictors, glm.logit1$y)
points(glm.logit1$linear.predictors, glm.logit1$fitted.values)

plot(glm.probit1$linear.predictors, glm.probit1$y)
points(glm.probit1$linear.predictors, glm.probit1$fitted.values)


#############################################
###### 2 Boston Housing Data

data(Boston); #this data is in MASS package
colnames(Boston) 
#column variable's names; the last variable "medv" is used as y

# dim[1] = num of observation dim [2] = num of column
size <- floor(dim(Boston)[1]*0.9); 

Boston_train <-Boston[sample(rownames(Boston),size, replace = FALSE),];

y <- Boston_train[,14]
x1 <- Boston_train[,1]
x2 <- Boston_train[,2]
x3 <- Boston_train[,3]
x4 <- Boston_train[,4]
x5 <- Boston_train[,5]
x6 <- Boston_train[,6]
x7 <- Boston_train[,7]
x8 <- Boston_train[,8]
x9 <- Boston_train[,9]
x10 <- Boston_train[,10]
x11 <- Boston_train[,11]
x12 <- Boston_train[,12]
x13 <- Boston_train[,13]


model <- glm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13)

mod<-step(model, direction="both", trace=1) #stepwise
p2 <- summary(mod)
p2 <- p2[1]   #this is the output model



############################################
##### 3 Credit Scoring Data
credit0 <- read.csv("credit0.csv", header=T) #load data file
colnames(credit0)                            #data names

nobs <-dim(credit0)[1]    #number of data points
m <- dim(credit0)[2]
myseq <- 1:nobs           #list from 1 to number of data points

nsub<-floor(nobs*0.8)     #80% of original data

subset<-sample(myseq, nsub, replace = F)   #sample original data
credit.train<-credit0[subset,]             #training data



y <- credit.train[,2];  #load data
x1 <- credit.train[,3]
x2 <- credit.train[,4]
x3 <- credit.train[,5]
x4 <- credit.train[,6]
x5 <- credit.train[,7]
x6 <- credit.train[,8]
x7 <- credit.train[,9]
x8 <- credit.train[,10]
x9 <- credit.train[,11]
x10 <- credit.train[,12]
x11 <- credit.train[,13]
x12 <- credit.train[,14]
x13 <- credit.train[,15]
x14 <- credit.train[,16]
x15 <- credit.train[,17]
x16 <- credit.train[,18]
x17 <- credit.train[,19]
x18 <- credit.train[,20]
x19 <- credit.train[,21]
x20 <- credit.train[,22]
x21 <- credit.train[,23]
x22 <- credit.train[,24]
x23 <- credit.train[,25]
x24 <- credit.train[,26]
x25 <- credit.train[,27]
x26 <- credit.train[,28]
x27 <- credit.train[,29]
x28 <- credit.train[,30]
x29 <- credit.train[,31]
x30 <- credit.train[,32]
x31 <- credit.train[,33]
x32 <- credit.train[,34]
x33 <- credit.train[,35]
x34 <- credit.train[,36]
x35 <- credit.train[,37]
x36 <- credit.train[,38]
x37 <- credit.train[,39]
x38 <- credit.train[,40]
x39 <- credit.train[,41]
x40 <- credit.train[,42]
x41 <- credit.train[,43]
x42 <- credit.train[,44]
x42 <- credit.train[,45]
x43 <- credit.train[,46]
x44 <- credit.train[,47]
x45 <- credit.train[,48]
x46 <- credit.train[,49]
x47 <- credit.train[,50]
x48 <- credit.train[,51]
x49 <- credit.train[,52]
x50 <- credit.train[,53]
x51 <- credit.train[,54]
x52 <- credit.train[,55]
x53 <- credit.train[,56]
x54 <- credit.train[,57]
x55 <- credit.train[,58]
x56 <- credit.train[,59]
x57 <- credit.train[,60]
x58 <- credit.train[,61]
x59 <- credit.train[,62]
x60 <- credit.train[,63]




credit.glm<-glm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34+x35+x36+x37+x38+x39+x40+x41+x42+x43+x44+x45+x46+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+x58+x59+x60,family=binomial(link="logit"))

cred <- step(credit.glm, direction="both", trace=1) #stepwise
p3 <- summary(cred)
p3 <- p3[1]   #this is the output model


##################################################################################
##### 4 Bankruptcy Data
library(MASS)
library(stats)
bankrupt <- read.csv("bankruptcy.csv", header=T)
colnames(bankrupt)  

nobs <-dim(bankrupt)[1]    #number of data points
myseq <- 1:nobs           #list from 1 to number of data points
m <- dim(bankrupt)[2]

nsub<-floor(nobs*0.9)     #90% of original data

subset<-sample(myseq, nsub, replace = F)   #sample original data
bankrupt.train<-bankrupt[subset,]             #training data



y <- bankrupt.train[,2];  #load data
x1 <- bankrupt.train[,4]
x2 <- bankrupt.train[,5]
x3 <- bankrupt.train[,6]
x4 <- bankrupt.train[,7]
x5 <- bankrupt.train[,8]
x6 <- bankrupt.train[,9]
x7 <- bankrupt.train[,10]



bankrupt.glm<-glm(y~x1+x2+x3+x4+x5+x6+x7, family=binomial(link="logit"));

bank<-step(bankrupt.glm, direction="both", trace=1) #stepwise
p4 <- summary(bank)
p4 <- p4[1]   #this is the output model

