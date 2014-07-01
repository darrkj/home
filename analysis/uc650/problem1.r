#### Hoemwork #2
# Problem 1
sink("problem1.txt", append=FALSE, split=FALSE)
library(MASS)
library(stats)

#part i
x1 <- rnorm(200) * .5 + 2                #make x1 and x2 have length 5000
x2 <- rnorm(200) * .1 - 1                #so later just use first n of them 

##### Data structures for models
lm_mod.array <- array(0, dim = c(100, 3))
lm_mse.array <- array(0, dim = c(100, 1))


#part ii
for (n in c(1:100)) 
{
    	e <- rnorm(200,sd=1)             #this is the error term
    	y <- 4 + .8 * x1 + 2 * x2 + e    #observed values to regress on

   	fit <- lm(y~x1+x2);              #simple linear regression with x1 x2
	lm_mod.array[n, 1] <- fit$coefficient[[1]]    #intercept
	lm_mod.array[n, 2] <- fit$coefficient[[2]]    #x1
	lm_mod.array[n, 3] <- fit$coefficient[[3]]    #x2

	summary <- summary(fit);                      #relavent values from the OLS
	lm_mse.array[n] <- (summary$sigma)^2       #MSE	
}

int <- 0
x1 <- 0
x2 <- 0
#part iii
for (n in c(1:100))
{
	int <- int + lm_mod.array[n, 1]
	x1 <- x1 + lm_mod.array[n, 2]
	x2 <- x2 + lm_mod.array[n, 3]
}
b0_avg <- int/100
b1_avg <- x1/100
b2_avg <- x2/100
b0_bias <- b0_avg - 5
b1_bias <- b1_avg - .9
b2_bias <- b2_avg - 2
int_diff <- 0
x1_diff <- 0
x2_diff <- 0
for (n in c(1, 100))
{
	int_diff <- (int_diff - b0_avg)^2
	x1_diff <- (x1_diff - b1_avg)^2
	x2_diff <- (x2_diff - b2_avg)^2
}

b0_var <- int_diff/100
b1_var <- x1_diff/100
b2_var <- x2_diff/100
b0_mse = b0_var + b0_bias^2
b1_mse = b1_var + b1_bias^2
b2_mse = b2_var + b2_bias^2

