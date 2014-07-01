library (class)

num.n <- 1000
num.u <- 10000
num.k <- 10

n <- rnorm (num.n, 50, 12)
n[n > 100] <- 100
n[n <   0] <-   0

u <- runif (num.u, 0, 100)

grid <- seq (0, 100, 4)

nh <- hist (n, breaks=grid, freq=FALSE, xlim=c(0, 100), col="orange", ylim=c(0, 0.15),
            main="CADE test using a Normal distribution as a target", xlab="Points",
            sub=sprintf ("%d Normal points, %d Uniform points, and K=%d", num.n, num.u, num.k))

uh <- hist (u ,breaks=grid, density=10, freq=FALSE, add=T)

# Using count instead of density gives more interesting results, since
# increasing num.u will make a difference. It also works more like KNN.

probs <- nh$count / (nh$count + uh$count)
probs <- ifelse (is.finite (probs), probs, 0)
probs <- probs / sum (probs)

# odds <- nh$count / uh$count
# odds <- ifelse (is.infinite (odds), 100, odds)
# odds <- odds / sum (odds)

train <- matrix (c(n, u), ncol=1)
test <- matrix (n, ncol=1)
lab <- c(rep ("Good", length (n)), rep ("Bad", length (u)))

# KNN's probability calculations are for the predicted class
# and thus will never go below 0.5 (at which point, it would
# switch to the other classification). So the ifelse is needed
# to make them comparable.

k <- knn (train, test, lab, k=10, prob=TRUE)
kprobs <- attr (k, "prob")
kprobs <- ifelse (k == "Good", kprobs, 1 - kprobs)
kprobs <- kprobs / 10    # Yeah, it's arbitrary to fit it into the same plot

lines (uh$mids - 0.5, probs,  type="h", col="red",   lwd=3)
points (n,       kprobs, col="blue", lwd=3)
# lines (uh$mids + 0.5, odds,   type="h", col="green",  lwd=3)
abline (h=0.05, lty=3, col="blue")

axis (4, seq (0, 0.1, 0.01), paste (seq (0, 100, 10), "%"), col="blue")

legend (70, 0.15, c("Normal Target", "Uniform Background", "Probability", "KNN (right axis)"),
        col=c("orange", "black", "red", "blue"), lwd=3)

# dcos <- function (x, m, s) (1 / (2 * s)) * (1 + cos (pi * (x - m) / s))
