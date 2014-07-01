###
# Partition data
# Returns three data frames, accessible from the 
# to call records at the end, output to an oject, and then set that
# object to objectname$traindata
##

partition <- function(data, trainpercent, validationpercent){
  rows <- nrow(data)
  trainrows <- trainpercent / 100 * rows
  validationrows <- validationpercent / 100 * rows
  testrows <- (100 - trainpercent - validationpercent) / 100 * rows
 
  data$RAND <- runif (nrow(data), min = 0, max = 1)  
  data <- data[order(data$RAND), ]
  traindata <- data[1:trainrows, ]
  validationdata <- data[(trainrows + 1):(trainrows + validationrows), ]
  testdata <- data[(trainrows + validationrows + 1):rows, ]
  outputdata <- list("traindata" = traindata, "validationdata" = 
                     validationdata, "testdata" = testdata)
}
