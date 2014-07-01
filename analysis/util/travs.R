###############################################################################
#
#       chunk
#
###############################################################################
#' Add chunks of time to a data set
#'
#'
#' @param data A data.frame with a date column.
#' @param freq how you want to group regions of time
#' @keywords timeseries, temporal
#' @author Kenny Darrell
#' @return the idx field which can be joined back onto the data.
#' @export
#' @examples
#' library(chRonos)
#' data(mvData)
#' max(chunk(mvData$date, 'year')$idx)
#' max(chunk(mvData$date, 'day')$idx)
#' names(chunk(mvData$date, 'day'))
#' nrow(chunk(mvData$date, 'day'))
#' TODO: chunk: add ability to create your own time spans.
#' TODO: chunk: should I auto join the idx to the data.frame

chunk <- function(data, freq) {
  data <- data.frame(date = as.Date(min(data):max(data)), idx = 1)
  potFreq <- c('year', 'month', 'week', 'day')
  x <- which(freq == potFreq)
  slices <- potFreq[1:x]
  for (i in 1:x) {
    data[, i+2] <- do.call(eval(potFreq[i]), list(data$date))
  }
  id <- (ncol(data) - x + 1):ncol(data)
  names(data)[id] <- slices
  # Get rid of date 
  z <- data[, c(2, id)]
  if (freq == 'year')  z <- unique(z[order(z$year), ])
  if (freq == 'month') z <- unique(z[order(z$year, z$month), ])
  if (freq == 'week')  z <- unique(z[order(z$year, z$month, z$week), ])
  if (freq == 'day')   z <- unique(z[order(z$year, z$month, z$week, z$day), ])
  # The increment in time id.
  z$idx <- 1:nrow(z)
  data$idx <- NULL
  # Merge the increment onto the whole data frame.
  d <- merge(unique(data), z, by = intersect(names(data), names(z)))
  # Drop the year, month, week, day cols that exist.
  d <- d[, !(colnames(d) %in% potFreq)]
  # Try to find a way to do this with expressions.
  return(d)
}


###############################################################################
#
#       ttply
#
###############################################################################
#' Apply function to data in chunks of time.
#'
#'
#' @param data The data frame of interest.
#' @param freq how you want to group regions of time
#' @param FUN THe functionto apply to the data at each group
#' @param lag how many chunks in each group
#' @keywords timeseries, temporal
#' @author Kenny Darrell
#' @return A list of the the outcome of each function.
#' @export
#' @examples
#' library(chRonos)
#' data(mvData)
#' ids <- chunk(mvData$date, 'month')
#' mvData1 <- merge(mvData, ids, by = 'date')
#' cc <- ttply(mvData1, function(x) length(unique(x$name)), lag = 5)
#' TODO: ttply: should I add chunk into this function.


ttply <- function(data, FUN, lag = 1000, ...) {
  result <- NULL
  for (i in unique(data$idx)) {
    subDATA <- data[data$idx <= i & data$idx >= i - lag, ]
    result <- c(result, FUN(subDATA))
  }  
  return(result)
}

# Temporally Trajected Automatic Validated Scheme


