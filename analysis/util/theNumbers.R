###############################################################################
#
# theNumbers.R
#
# Author - Kenny Darrell
#
# Purpose - This is a function that can be used to 
# pull data from the theNumbers.com website.  It
# will create a dataset that captures historical box
# office performance.
#
# 
###############################################################################

#' Pull Movie Data
#' 
#' 
#' @return data.frame
#' @keywords data
#' @export
#'

numbers <- function(start = "2013-05-01") {
  # Load packages needed in this function.
  library(XML)
  library(lubridate)
  # This is useful for imported data.
  options(stringsAsFactors = FALSE)
  
  # This is the most updated date on the site, 4 day lag.
  finish <- today() - 2
  # This is the point in time to start pulling from.
  start <- as.Date(start)
  # Check to make sure there are days to grab data from.
  stopifnot(start < finish)
  
  # Source where data comes from.  
  api <- "http://www.the-numbers.com/box-office-chart/daily/"
  # Init list
  g <- vector("list", length = as.numeric(finish - start))
  
  # Construct loop to go through days  
  for  (j in seq(finish - start)) {
    print(start)
    # Reformat date to website format.
    x <- gsub('-', '/', as.character(start), fixed = T)
    # Create string of HTML source of data for current day.
    site <- paste(api, x, sep = "")
    # Read data from that site.
    mList <- readHTMLTable(site)[[4]]
    # Strip out the data that I want, no headers and append to list.
    g[[j]] <- cbind(start, mList)
    # Increment to the next day.
    start <- start + 1
  }
  mvData <- recurBind(g)[[1]]

  # Remove values that are no longer needed.
  rm(x, mList, finish, site, start, api, g, j)
  
  # Rename varaibles to have more meaning. 
  names(mvData) <- c("date", "todRank", "yesRank", "name", "Distributor", 
                     "Genre", "Gross", "Change", "Thtrs", "PerThtr", 
                     "TotalGross", "Day")
  
  # Add weekday variable
  mvData$weekday <- wday(mvData$date, label = TRUE)
  
  # Function used to strip characters used for web visualization.
  mojoClean <- function(x) {
    return(as.numeric(gsub("[)($,+%Â]", "", x, fixed = F)))  
  }
  
  # Clean the characters out of the fields that need to be numbers.
  suppressWarnings({
    mvData[, 2]  <- mojoClean(mvData[, 2])
    mvData[, 3]  <- mojoClean(mvData[, 3])
    mvData[, 7]  <- mojoClean(mvData[, 7])
    mvData[, 8]  <- mojoClean(mvData[, 8])
    mvData[, 9]  <- mojoClean(mvData[, 9])
    mvData[, 10] <- mojoClean(mvData[, 10])
    mvData[, 11] <- mojoClean(mvData[, 11]) 
  }) 

  # Order name then day into its showing.
  mvData <- mvData[with(mvData, order(name, Day)), ]
  return(mvData)
}


#' The numbers refresh
#' 
#' 
#' @return data.frame
#' @keywords data
#' @export
#' 

numbersRef <- function() {
  library(chRonos)
  data(nbData)
  # Start from the day after that most recent day.
  start <- max(nbData$date) + 1
  # Remove this data.
  return(numbers(start))
}




#' The Numbers Data
#'
#' @docType data
#' @name nbData
NULL
