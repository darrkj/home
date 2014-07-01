reps <- 50 
sd = .25
k=2

data <- gen2dData(10000, 0.5)

N <- nrow(data)
#create list of random number, distribute across columns of matrix
perturbations <- matrix(rnorm(N * reps, 0, sd), nrow = N)
#create matrix that of real data plus noise above
yeMat <- matrix(data$y, nrow = N, ncol = reps, byrow = FALSE) + perturbations
#model of real data
baseModel <- modelTrainer(data, k)
#predicted values from real model
yHat <- predict(baseModel, data)
#initialize matrix for real plus noise predictions
yeHatMat <- matrix(NA, nrow = N, ncol = reps)

for (i in 1:reps) {
  #substitute ith col of noisey data for y
  data$y <- yeMat[,i]
  #train model on y plus noise
  modelPerturbed <- modelTrainer(data, k)
  #predict noisey data
  yeHatMat[,i] <- predict(modelPerturbed, data)
}
#init generalized degree of freedom coef's
gdf <- c(NA, N)
for (i in 1:N) {
  gdf[i] <- lm(yeHatMat[i,] - yHat[i] ~ perturbations[i,])$coefficients[2]
}