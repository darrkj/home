library(manipulate)
library(ggplot2)
library(reshape2)
library(plyr)
library(plotrix)
library(igraph)

# Max out addresable memory for 32 bit software.
memory.limit(4095)

# Useful for data imports.
options(stringsAsFactors = FALSE)

`%ni%` <- Negate(`%in%`)
###############################################################################
# Function to capture columns that are completely missing.
allMiss <- function(data) {
  # init for seq concat
  x <- NULL
  rows <- nrow(data)
  # Go through each field.
  for (col in seq(ncol(data))) {
    # Count the number of missing values.
    y <- sum(is.na(data[,col]))
    if (y == rows) {
      # If they are all missing add the index
      # to the list of fields to remove.
      x <- c(x, col)
    }
  }
  return(data[,-x])
}

###############################################################################
# This function finds the lowest common denominater for of
# fields for a group of sets.
setLCD <- function(data) {
  # Initialise contents
  lcd <- names(data[[1]])
  # Loop through all sets
  for (i in seq(length(data))) {
    # Intersect with the current LCD
    lcd <- intersect(lcd, names(data[[i]]))
  }
  return(lcd)
}

###############################################################################
# Function to roll up list of data sets, removing fields that
# are not in the LCD.
keepLCD <- function(set, lcd) {
  # These are the fields that will be kept for this set.
  keep <- names(set[[1]]) %in% lcd
  # initialize data frame with first month and the set of needed fields. 
  data <- set[[1]][,keep]
  # Loop through each month, an item in the list.
  for (i in seq(length(set))[-1]) {
    keep <- names(set[[i]]) %in% lcd
    # This appends months together and makes unseen fields NA.
    data <- rbind.fill(data, set[[i]][,keep])
    gc()
  }
  return(data)
}
###############################################################################
# This function does some date transformations.
# It is very verbose and does soemthing very specific to the case in which
# it is being applied.  No chance of reuse.
finalize <- function(data) {
  data$year <- as.numeric(substr(data$time, 1, 4))
  
  data$month <- sapply(data$time, function(x) switch(x,
        "2011_Oct" = 1, "2011_Nov" = 2,   "2011_Dec" = 3,  "2011_Jan" = 4,
        "2011_Feb" = 5, "2011_Mar" = 6,   "2011_Apr" = 7,  "2011_May" = 8,
        "2011_Jun" = 9, "2011_Jul" = 10,  "2011_Aug" = 11, "2011_Sep" = 12,
        "2012_Oct" = 13, "2012_Nov" = 14, "2012_Dec" = 15, "2012_Jan" = 16,
        "2012_Feb" = 17, "2012_Mar" = 18, "2012_Apr" = 19, "2012_May" = 20,
        "2012_Jun" = 21, "2012_Jul" = 22, "2012_Aug" = 23, "2012_Sep" = 24,
        "2013_Oct" = 25, "2013_Nov" = 26, "2013_Dec" = 27, NA))
  
  data$qtr <- ceiling(data$month / 3)
  
  return(data)
}
###############################################################################
# Function to get the metric score for a question
# through some period of time.  Assumes bsn data is in memory,
# should add a check for the case that it is not.
metBSN <- function(metric, by, ret = TRUE, graph = TRUE) {
  # Setup the period to group on, and the values in that group.
  if(by == "qtr") {
    freq <- sort(unique(bsn$qtr))
  } else if (by == "month") {
    freq <- sort(unique(bsn$month))
  } else {
    freq <- sort(unique(bsn$year))
  }
  # Subset the data.
  surv <- bsn[, c(metric, by)]
  x <- NULL
  # Loop through each period.
  for (i in freq) {
    sx <- surv[surv[,2] == i,1]
    sx <- sx[!is.na(sx)]
    total <- length(sx)
    met <- length(sx[sx %in% c(4, 5)])
    x <- c(x, met/total)
  }
  if (graph) {
    plot(ylim = c(min(x)-.1, max(x)+.1), x, 
         xlab = "Period", ylab = "Score",
         main = metric)
  }
  if (ret) {
    return(x)
  }
}

# Function to clean zip codes. Don't need anymore, GeoZip3 is this.
Zip.Codify <- function(zip) {
  # Turn Zipcode into character field, if not already. 
  zip <- as.character(zip)
  # Replace ### with 00### 
  zip <- ifelse(nchar(zip) == 3, paste("00", zip, sep=""), zip)
  # Replace #### with 0#### 
  zip <- ifelse(nchar(zip) == 4, paste("0", zip, sep=""), zip)  
}



###############################################################################
# From 
# http://stackoverflow.com/questions/9614433/creating-radar-
#   chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
# Radar plot function wrapper.
radarFix <- function(d){
  # assuming the passed in data frame 
  # includes only variables you would like plotted and segment label
  d$seg <- as.factor(d$seg)
  # find increment
  angles <- seq(from = 0, to = 2*pi, by = (2*pi)/(ncol(d)-2))
  # create graph data frame
  graphData <- data.frame(seg = "", x = 0,y = 0)
  graphData <- graphData[-1,]
  for(i in levels(d$seg)) {
    segData <- subset(d, seg == i)
    for(j in c(2:(ncol(d)-1))) {
      # set minimum value such that it occurs at 0. 
      # (center the data at -3 sd)
      segData[,j] <- segData[,j]+3
      graphData <- rbind(graphData, data.frame(seg = i, 
                        x = segData[,j]*cos(angles[j-1]),
                        y = segData[,j]*sin(angles[j-1])))
    }
    # completes the connection
    graphData <- rbind(graphData, data.frame(seg = i, 
                        x = segData[,2]*cos(angles[1]),
                        y = segData[,2]*sin(angles[1])))
  }
  return(graphData)
}

###############################################################################
# Function to get the metric score for a question
# through some period of time.
mets <- function(survey, metric, by, ret = TRUE, graph = TRUE) {
  keep <- c(1, 2, 3, 4, 5, 6, 7)
  score <- c(1, 2)
  
  if(by == "qtr") {
    weight <- "QuarterlyWeight"
    freq <- sort(unique(survey$qtr))
  } else if (by == "month") {
    weight <- "MonthlyWeight"
    freq <- sort(unique(survey$month))
  } else {
    weight <- "YTDWeight"
    freq <- sort(unique(survey$year))
  }
  surv <- survey[, c(metric, by, weight)]
  
  x <- NULL
  for (i in freq) {
    sx <- surv[surv[,2] == i,]
    total <- sum(sx[sx[,1] %in% keep,][,3])
    met <- sum(sx[sx[,1] %in% score,][,3])
    x <- c(x, met/total)
  }
  # Optional graph.
  if (graph) {
    plot(ylim = c(min(x)-.1, max(x)+.1), x, 
         xlab = "Period", ylab = "Score",
         main = metric)
  }
  # Optional return array.
  if (ret) {
    return(x)
  }
}

###############################################################################
# Idea from 
# http://stats.stackexchange.com/questions/2455/
#   how-to-make-age-pyramid-like-plot-in-r
# Create a pyramid plot, or back to back bar chart.
b2bPlot <- function(data, question, num = 5) {
  yes <- grpRes(data, question, "1")$avg
  no  <- grpRes(data, question, "0")$avg
  idx <- rev(order(abs(yes-no)))[1:num]
  mar = pyramid.plot(
    rev(yes[idx]), rev(no[idx]), labels=rev(ques[idx]), 
    top.labels = c("Yes","Question","No"),
    main = question, lxcol="red", rxcol="blue",
    gap = 0.5, show.values = TRUE)
}

###############################################################################
# Clean odd aspects of the survey data.
clean <- function(data, rmv = c(), join = c(3)) {
  data[data %in% union(c(0, 9), rmv)] <- NA
  if ( length(join) > 1) {
    data <- ifelse(data > min(join), data-1, data)
  }
  return(data)
}
###############################################################################
# Clean odd aspects of the survey data.
cleanBSN <- function(data) {
  # Create not-in operator
  `%ni%` <- Negate(`%in%`)
  # Make values outside of range missing
  data[data %ni% seq(5)] <- NA
  # Return the with this reverse imputation applied.
  return(data)
}

###############################################################################
# Function to create grouped bar charts.
# There is no return, only called for side effects of creating a plot.
groupBar <- function(data, q) {
  # Initialize data frame.
  df <- data.frame(Question = NA, Score = NA, group = NA)
  # Loop through each value in for given field.
  for (i in unique(data[,q])) {
    # Only use cases where there are enough observations.
    if (sum(data[,q] == i, na.rm = TRUE) > 2) {
      # Add each of the score for question by group to data frame.
      df <- rbind(df, data.frame(Question = ques, 
                                 Score = grpRes(data, q, i)$avg, 
                                 group = i))
    }
  }
  # Remove the initial missing row created from init.
  df <- df[2:nrow(df),]
  # Turn group into factor variable.
  df$group <- factor(df$group)
  # This creates a bar chart for the whole survey.
  p <- ggplot(df, aes(fill=group, y=Score, x=Question))
  p + geom_bar(position="dodge", stat="identity")
}
###############################################################################
# Function that can get a score for each question.
# Takes a data set, a field of relavence and a specific value
# from that field.  Assumes there is a list of fields
# in the form of ques.
grpRes <- function(data, demog, answer) {
  # Subset where the variable has the instance under consideration.
  data <- data[data[,demog] == answer,][,ques]
  # Initialze return array.
  avg <- NULL
  var <- NULL
  # Loop through each column of the input data.
  for (i in 1:ncol(data)) {
    # Calculate the mean and var and append to the growing list.
    avg <- c(avg, mean(data[,i], na.rm = TRUE))
    var <- c(var, sd(data[,i], na.rm = TRUE))
  }
  # Group together
  result <- list(avg=avg, var=var)
  return(result)
}
###############################################################################
# Function used to get an understanding of the data.
strNA <- function(data) {
  for (i in 1:ncol(data)) {
    name <- names(data)[i]
    type <- class(data[,i])
    sample <- data[!is.na(data[,i]),i][1:10]
    miss <- sum(is.na(data[,i]))
    cat("$ ", name, " : ", type, " ... ",
        sample, " Missing ", miss, "\n")
  }
}
# Better version.
strNA <- function(data) {
  cat(nrow(data), "\n")
  for (i in 1:ncol(data)) {
    name <- substr(names(data)[i], 1, 14)
    type <- class(data[,i])
    uniq <- length(unique(data[,i]))
    miss <- sum(is.na(data[,i]))
    space <- "              "
    space1 <- substr(space, 1, (14-nchar(name)))
    space2 <- substr(space, 1, (10-nchar(type)))
    space3 <- substr(space, 1, (6-nchar(as.character(uniq))))
    cat("$ ", name, space1, " : ", type, space2, 
        ":", "Unique", uniq, space3, "Missing ", 
        miss, "\n")
  }
}

###############################################################################