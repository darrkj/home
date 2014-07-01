###############################################################################
#
#     AllenTmpLogic
#
###############################################################################

#' Before
#'  
#' @author Kenny Darrell
#' @return boolean
#' @param x first time series
#' @param y second time series
#' @keywords temporal logic
#' @references http://en.wikipedia.org/wiki/Allen's_interval_algebra
#' @export
#' @rdname before
#' @examples
#' x <- as.Date("2013-04-20") + (-10:10)
#' y <- as.Date("2013-04-20") + (0:20)
#' 
#' x %before% y
#' x %before% y[15:20]

"%before%" <- function(x, y) max(x) < min(y)

#' After
#' 
#' I created this one for completeness
#'  
#' @author Kenny Darrell
#' @return boolean
#' @param x first time series
#' @param y second time series
#' @keywords temporal logic
#' @references http://en.wikipedia.org/wiki/Allen's_interval_algebra
#' @export
#' @rdname after
#' @examples
#' x <- as.Date("2013-04-20") + (-10:10)
#' y <- as.Date("2013-04-20") + (0:20)
#' 
#' x %after% y
#' x %after% y[15:20]

"%after%" <- function(x, y) min(x) > max(y)

#' Meets
#' 
#' X meets Y (i stands for inverse)
#'  
#' @author Kenny Darrell
#' @return boolean
#' @param x first time series
#' @param y second time series
#' @keywords temporal logic
#' @references http://en.wikipedia.org/wiki/Allen's_interval_algebra
#' @export
#' @rdname meets
#' @examples
#' x <- as.Date("2013-04-20") + (-10:10)
#' y <- as.Date("2013-04-20") + (0:20)
#' 
#' x %meets% y
#' x %meets% y[12:20]

"%meets%" <- function(x, y) as.numeric(min(y) - max(x)) == 1

#' Overlaps
#' 
#' X overlaps with Y
#'  
#' @author Kenny Darrell
#' @return boolean
#' @param x first time series
#' @param y second time series
#' @keywords temporal logic
#' @references http://en.wikipedia.org/wiki/Allen's_interval_algebra
#' @export
#' @rdname overlaps
#' @examples
#' x <- as.Date("2013-04-20") + (-10:10)
#' y <- as.Date("2013-04-20") + (0:20)
#' 
#' x %overlaps% y
#' x %overlaps% y[12:20]

"%overlaps%" <- function(x, y) {
  max(x) > min(y) & min(x) < min(y) & max(x) < max(y) |
    max(y) > min(x) & min(y) < min(x) & max(y) < max(x)
}

#' Start
#' 
#' X starts Y
#'  
#' @author Kenny Darrell
#' @return boolean
#' @param x first time series
#' @param y second time series
#' @keywords temporal logic
#' @references http://en.wikipedia.org/wiki/Allen's_interval_algebra
#' @export
#' @rdname starts
#' @examples
#' x <- as.Date("2013-04-20") + (-10:10)
#' y <- as.Date("2013-04-20") + (0:20)
#' 
#' x %starts% y
#' x[11:20] %starts% y
 
"%starts%" <- function(x, y) min(x) == min(y) & max(x) < max(y)


#' During
#' 
#' X during Y
#'  
#' @author Kenny Darrell
#' @return boolean
#' @param x first time series
#' @param y second time series
#' @keywords temporal logic
#' @references http://en.wikipedia.org/wiki/Allen's_interval_algebra
#' @export
#' @rdname during
#' @examples
#' x <- as.Date("2013-04-20") + (-10:10)
#' y <- as.Date("2013-04-20") + (0:20)
#' 
#' x %during% y
#' x[12:20] %during% y

"%during%" <- function(x, y) min(x) > min(y) & max(x) < max(y)


#' Finishes
#' 
#' X finishes Y
#'  
#' @author Kenny Darrell
#' @return boolean
#' @param x first time series
#' @param y second time series
#' @keywords temporal logic
#' @references http://en.wikipedia.org/wiki/Allen's_interval_algebra
#' @export
#' @rdname finishes
#' @examples
#' x <- as.Date("2013-04-20") + (-10:10)
#' y <- as.Date("2013-04-20") + (0:20)
#' 
#' x %finishes% y
#' x[11:20] %finishes% y

"%finishes%" <- function(x, y) min(x) > min(y) & max(x) == max(y)


#' Equal
#' 
#' X is equal to Y
#'  
#' @author Kenny Darrell
#' @return boolean
#' @param x first time series
#' @param y second time series
#' @keywords temporal logic
#' @references http://en.wikipedia.org/wiki/Allen's_interval_algebra
#' @export
#' @rdname equal
#' @examples
#' x <- as.Date("2013-04-20") + (-10:10)
#' y <- as.Date("2013-04-20") + (0:20)
#' 
#' x %equal% y
#' x[11:20] %equal% y[1:9]

"%equal%" <- function(x, y) min(x) == min(y) & max(x) == max(y)


# 
# a <- as.Date("2013-04-20") + (-20:0)
# b <- as.Date("2013-04-20") + (0:20)
# c <- as.Date("2013-04-20") + (1:20)
# d <- as.Date("2013-04-20") + (10:20)
# e <- as.Date("2013-04-20") + (-1:10)
# f <- as.Date("2013-04-20") + (0:20)
# 
# 
# library(zoo)
# 
# c <- (1:500)/20
# x <- as.Date("2013-04-20") + (-10:10)
# y <- as.Date("2013-04-20") + (0:20)
# 
# dd.Data <- zoo(sin(c)+runif(500), x)
# ee.Data <- zoo(sin(c)+runif(500), y)

# Similar to 
# http://en.wikipedia.org/wiki/Region_Connection_Calculus
# http://en.wikipedia.org/wiki/Temporal_logic
# 
# disconnected (DC)
# externally connected (EC)
# equal (EQ)
# partially overlapping (PO)
# tangential proper part (TPP)
# tangential proper part inverse (TPPi)
# non-tangential proper part (NTPP)
# non-tangential proper part inverse (NTPPi)
