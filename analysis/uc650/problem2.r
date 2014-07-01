sink("problem2.txt", append=FALSE, split=FALSE)
library(gam)

k <- 500
x1 <- runif(k, min=0, max=1)
x2 <- NULL

#part i
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

gam.out <- gam(y ~ s(x1) + x2)
gam.i <- summary(gam.out)


#part ii
k <- 1000
x1 <- runif(k, min=0, max=1)
x2 <- NULL


for(i in 1:k)
{
	if ((i/2-floor(i/2) > 0)) 
		x2[i] <- 0
	else
		x2[i] <- 1
}

logitp <-  sin(-1 + 5.5 * x1) -0.3 * x2
p <- plogis(logitp)

y <- NULL

for(i in 1:k)
{
	y[i] <- rbinom(1, 1, p[i])
}

glm.logit <- glm(y ~ x1 + x2, family = binomial(link=logit))
glm.ii <- summary(glm.logit)
gam.out <- gam(y ~ s(x1) + x2)
gam.ii <- summary(gam.out)


#plot(glm.logit$linear.predictors, glm.logit$y)
#points(glm.logit$linear.predictors, glm.logit$fitted.values)
#
#plot(glm.probit$linear.predictors, glm.probit$y)
#points(glm.probit$linear.predictors, glm.probit$fitted.values)
