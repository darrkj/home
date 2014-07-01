###############################################################################
#
#     mahalOutlier
#
###############################################################################
#' Calculate multi-dimensional outlier
#'
#' This function will return a copy of the input data frame
#' with its mahalanobis distance as a new column named dist
#' along with another column called outlier which is a one
#' or zero which denotes whether the row is outside of the 
#' cutoff.
#' 
#' This function will take a dataset as an argument and
#' calculate the \code{\link{mahalanobis}} distance from every observation
#' to the 'normal' observation.  This distance will be used
#' along with the number of dimisions to calculate an n-diminsional
#' outlier.  This is done using the Chi-Square \code{\link{qchisq}}, distribution with
#' n - 1 degrees of freedom and cumilative probability value given as an arguement.
#' 
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @return data.frame
#' @param dataset Data under consideration.
#' @param prop The probability cutoff.
#' @keywords mahalanobis outlier
#' @references http://blogs.sas.com/content/iml/2012/03/23/the-curse-of-dimensionality/
#' @export
#' @examples
#' # This works 
#' y <- mahalOutlier(orangeCar.train)
#' 
#' # This works better as these are more normalish
#' y <- mahalOutlier(orangeCar.train[,19:26])
#' 
#' # Example use of argument
#' y <- mahalOutlier

mahalOutlier <- function(dataset, prop = .975) {
  # TODO: mahalOutlier: Add better check on numeric fields. They could be id, or one/zero.
  # Pull out numeric variables in data frame.
  nums <- sapply(dataset, is.numeric)
 
  # Create new dataframe with only these numeric varaibles.
  data <- dataset[, nums]
  
  # Calculate the mahalanobis distance for each entry.
  dataset$dist <- mahalanobis(data, colMeans(data), cov(data))
  
  # This is the cutoff distance to be considered an outlier.
  dataset$cut <- sqrt(qchisq(prop, ncol(data) - 1))
  
  # Generate the ids for the outliers.
  id <- seq(1, nrow(data))[distances > cutoff]

  # Add one or zero to denote outliers.
  dataset$outlier <- 0
  dataset$outlier[id] <- 1
  
  return(dataset)
}
