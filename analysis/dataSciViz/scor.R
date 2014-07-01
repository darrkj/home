score <- function(dataSet, field, min=0, max=10, floor, ceil) {
  name <- paste(names(dataSet)[field], "score", sep="_")


  score <- (((dataSet[,field] - floor) * (max - min)) / (ceil - floor)) + min
  score <- ifelse(score > max, max, score)
  score <- ifelse(score < min, min, score)

  dataSet[,ncol(dataSet)+1] <- score
  names(dataSet)[ncol(dataSet)] <- name

  plot(dataSet[,field], dataSet[,ncol(dataSet)],
       xlab = eval(names(dataSet)[field]), 
       ylab = eval(names(dataSet)[ncol(dataSet)])
  )
#add defualt to not plot, ex plot = 1
  return (dataSet)
}


filter <- function(dataSet, field, stddev) {
  x <- mean(dataSet[,field]) + stddev * sd(dataSet[,field])
  dataSet[dataSet[,field] > x,]
}