###
#
# TODO: targetshuffling: Add targetshuffling to export namespace.
##

targetshuffling <- function(mydata, targetvariable) {
  mydata$RAND <- runif (nrow(mydata), min = 0, max = 1)
  mydata2 <- mydata[order(mydata$RAND), ]
  mydata$ROWNAMES <- row.names(mydata)
  row.names(mydata2) <- NULL
  mydata2$ROWNAMES <- row.names(mydata2)
  mydata$RAND <- NULL
  mydata[[targetvariable]] <- NULL
  mydata2 <- mydata2[, c(targetvariable, "ROWNAMES")]
  mydata3 <- merge(mydata, mydata2)
  mydata3$ROWNAMES <- NULL
  return(mydata3)
}


#' Implementation of target reshuffling 
#'
#' This function will take a data frame and shuffle
#' the target variable.
#' 
#' @author Sarah Will <will@@datamininglab.com>
#' @return data.frame
#' 
#' @param mydata Input data frame
#' @param targetvariable variable to shuffle
#' 
#' @keywords target reshuffle data mining
#' @export
#' @examples
#' data(orangeCar.train)
#' x <- shuffle(orangeCar.train, "IsBadBuy")
#' 

shuffle <- function(target, ...) {
  return(target[sample(length(target), ...)])
}

# library(boRg)
# data(orangeCar.train)
# data(orangeCar.test)
# 
# 
# system.time(
#   x <- shuffle(orangeCar.train, "IsBadBuy")
# )
# system.time(
#   x <- targetshuffling(orangeCar.train, "IsBadBuy")
# )
