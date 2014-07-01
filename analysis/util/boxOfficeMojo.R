###############################################################################
#
# boxOfficeMojo.R
#
# Author - Kenny Darrell
#
# Purpose - This is a function that can be used to 
# pull data from the boxofficemojo.com website.  It
# will create a dataset that captures historical box
# office performance.
#
# Dependency - If it is ran in refresh mode it needs
# an existing file to append to, part of needed work below
# is removing ths dependency.
#
# Using this function with ref = FALSE (F or 0) and 
# it will perform an inital pull of data.  It will take a while
# as it is pinging the boxofficemojo site once for every day 
# for about twenty years.  Calling it with no argument or ref = T
# and it will perform a refresh, update the data with the most
# recent values that have occured since it was last ran.  
#
# TODO: Should add a check, if it is a refresh then check the 
# file existence, otherwise do the initial pull.
# TODO: Make it able to accept explicit time range to add
# to current set of data.
# TODO: From working on theNumbers data pull it seems as though
# you can make the correct name of the dataframe up front and
# an not have to rename it later.  Delete renaming code and move
# those names to the init dataframe preallocation code.
# 
###############################################################################


#' Pull Movie Data
#' 
#' 
#' @return data.frame
#' @keywords data
#' @export
#'

mojo <- function(start = "2013-05-01") {
  # Load packages needed in this function.
  library(XML)
  library(lubridate)
  # This is useful for imported data.
  options(stringsAsFactors = FALSE)
  
  # This is the most updated date on the site, 4 day lag.
  finish <- today() - 4
  # This is the point in time to start pulling from.
  start <- as.Date(start)
  # Check to make sure there are days to grab data from.
  stopifnot(start < finish)

  # Source where data comes from.  
  api <- "http://www.boxofficemojo.com/daily/chart/?view=1day&sortdate="
  # Init list
  g <- vector("list", length = as.numeric(finish - start))
  
  # Construct loop to go through days
  for  (j in seq(finish - start)) {
    print(start)
    # Create string of HTML source of data for current day.
    site <- paste(api, start, "&p=.htm", sep = "")
    # Read data from that site.
    mList <- readHTMLTable(site)[[10]]
    # Strip out the data that I want, no headers and append to list.
    g[[j]] <- cbind(start, mList[4:(nrow(mList) - 1), ])
    # Increment to the next day.
    start <- start + 1
  }
  mvData <- recurBind(g)[[1]]
  
  # Remove values that are no longer needed.
  rm(mList, finish, site, start, api, g)
    
  # Rename varaibles to have more meaning. 
  names(mvData) <- c("date", "td", "yd", "name", "studio", "Daily", "peru", 
                     "perd", "Theaters", "Avg", "Gross", "Day")
    
  # Add weekday variable
  mvData$weekday <- wday(mvData$date, label = TRUE)
    
  # Function used to strip characters used for web visualization.
  mojoClean <- function(x) {
    return(as.numeric(gsub("[$,+%]", "", x, fixed = F)))  
  }
    
  # Clean the characters out of the fields that need to be numbers.
  mvData$daily    <- mojoClean(mvData$daily)
  mvData$Theaters <- mojoClean(mvData$Theaters)
  mvData$Avg      <- mojoClean(mvData$Avg)
  mvData$Gross    <- mojoClean(mvData$Gross)
  mvData$Day      <- mojoClean(mvData$Day)
    
  # These can have "-" in them from there first day
  # so the NA coercion warning is supressed.
  suppressWarnings({ 
    mvData$peru <- mojoClean(mvData$peru) / 100 
    mvData$perd <- mojoClean(mvData$perd) / 100
    mvData$td   <- mojoClean(mvData$td)
    mvData$yd   <- mojoClean(mvData$yd)
  }) 
    
  # Order name then day into its showing.
  mvData <- mvData[with(mvData, order(name, Day)), ]
  return(mvData)
}

#' Box office mojo refresh
#' 
#' 
#' @return data.frame
#' @keywords data
#' @export
#' 

mojoRef <- function() {
  library(chRonos)
  data(mvData)
  # Start from the day after that most recent day.
  start <- max(mvData$date) + 1
  # Remove this data.
  return(mojo(start))
}


#' Box Office Mojo Data
#'
#' @docType data
#' @name mvData
NULL
