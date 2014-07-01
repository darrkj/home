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
#' "%before%" <- function(x, y) max(x) < min(y)
#' 
#' date1 <- as.POSIXct("2009-03-08 01:59:59")
#' date2 <- as.POSIXct("2009-03-29 12:00:00")
#' x <- new_interval(date2, date1)
#' # 2000-02-29 12:00:00 CST--2009-03-08 01:59:59 CST
#' date3 <- as.POSIXct("2004-03-08 01:59:59")
#' date4 <- as.POSIXct("2000-02-29 12:00:00")
#' y <- new_interval(date3, date4)
#' # 2009-03-08 01:59:59 CST--2000-02-29 12:00:00 CST
#' x %before% y
#' y %before% x

'%before%' <- function(x, y) {
  max(x@start, x@start + x@.Data) < min(y@start, y@start + y@.Data)
}

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
#' y %after% x

"%after%" <- function(x, y) {
  min(x@start, x@start + x@.Data) > max(y@start, y@start + y@.Data)
}

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

"%during%" <- function(x, y) x %within% y


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

"%finishes%" <- function(x, y) {
  x@start != y@start | (x@start + x@.Data) == (y@start + y@.Data)
}

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

"%equal%" <- function(x, y) {
  x@start == y@start & (x@start + x@.Data) == (y@start + y@.Data)
}


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






library(lubridate)
new_interval(ymd(20090201), ymd(20090101))
# 2009-02-01 UTC--2009-01-01 UTC

date1 <- as.POSIXct("2009-03-08 01:59:59")
date2 <- as.POSIXct("2009-03-29 12:00:00")
x <- new_interval(date2, date1)
# 2000-02-29 12:00:00 CST--2009-03-08 01:59:59 CST
date3 <- as.POSIXct("2004-03-08 01:59:59")
date4 <- as.POSIXct("2000-02-29 12:00:00")
y <- new_interval(date3, date4)
# 2009-03-08 01:59:59 CST--2000-02-29 12:00:00 CST

span <- new_interval(ymd(20090101), ymd(20090201))
# 2009-01-01 UTC--2009-02-01 UTC

"%before%" <- function(x, y) max(x@start, x@start + x@.Data) < min(y@start, y@start + y@.Data)

"%before%" <- function(x, y) {
  UseMethod("%before%", x)
}

%before%.Interval <- function(x, y) {
  max(x@start, x@start + x@.Data) < min(y@start, y@start + y@.Data)
}

a <- as.Date("2013-04-20") + (-10:10)
b <- as.Date("2013-04-20") + (0:20)

"%before%".Date <- function(x, y) {
  max(x) < min(y)
}

