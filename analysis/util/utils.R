
#' Capture items which needs more work 
#'
#' This function will parse all of the code files and add
#' any mention of the todo tag to a file called
#' todo.txt.  This can be used for adding notes to .
#' for the future.
#' 
#' @author Kenny Darrell
#' @return NULL Ran only for side effects.
#' 
#' @param NULL Ran only for side effects.
#' 
#' @keywords tools
#' @export
#' @examples
#' TODO()
#' readTODO()
#'


TODO <- function() {
  # List of files in the R code directory.
  files <- list.files(path = "./R")
  
  # Create todo file sink to catch text being reported.
  sink("todo.txt", append = FALSE)
  
  # Loop over each file.
  for (i in grep(".R|.r", x = files)) {
    # This is the text in the file.
    text <- file(description = paste("R/", files[i], sep = ""))
    # The lines that have "TODO\:" in them.
    writeOut <- grep("TODO:", readLines(text), value = T)
    # Remove white space and leading text.
    writeOut <- substr(writeOut, regexpr(":", writeOut) + 2, nchar(writeOut))
    
    # Write this text to this file.
    if (length(writeOut) > 0) {
      for (x in writeOut) cat(files[i], ":", x, " \n", sep = "")
    }
    close(text)
  }
  # Turn this sink off, back to defualt console.
  sink()
}

#' Display contents of todo.txt 
#'
#' This function will show you what is contained in the todo.txt file
#' that is created from use of the \code{\link{TODO}} function.
#' @author Kenny Darrell
#' @return data.frame
#' 
#' @param NULL
#' 
#' @keywords tools
#' @export
#' @examples
#' TODO()
#' readTODO()
#'

readTODO <- function() {
  options(stringsAsFactors = FALSE)
  # Read the todo file into the workspace.
  list <- read.csv("todo.txt", header = FALSE, sep = ":")
  
  # Assign names to the two sections.
  names(list) <- c("File", "Function", "Action")
  
  # Get rid of redundent items. 
  list <- unique(list)
  # Return this list of items.
  return(list)
}


#' Initialize data frame 
#'
#' This function will 
#' 
#' @author Kenny Darrell
#' @return data.frame
#' 
#' @param name names of columns
#' @param row how man rows in the data frame
#' 
#' @keywords tools
#' @export
#'

initDF <- function(name, rows) {
  # String which start the data frame istantiation.
  init <- "df <- data.frame("
  for (i in name) {
    init <- paste(init, i, " = rep(NA, ", rows, "), ", sep = "")
  }
  init <- substr(init, 1, nchar(init)-2)
  init <- paste(init, ")", sep = "")
  eval(parse(text = init))
  return(df)
}



#' Recursive row binding 
#'
#' Don't carry huge sets around,
#' has divide and conquer on memory usage, large set only appear
#' at the top level of the recursion. 
#' 
#' @author Kenny Darrell
#' @return data.frame
#' 
#' @param dList List of data frames
#' @param row how man rows in the data frame
#' 
#' @keywords tools
#' @export
#'

recurBind <- function(dList) {
  len <- length(dList) / 2
  # Preallocate list for small improvement.
  data <- vector("list", len)
  j <- 1
  for (i in seq(len)) {
    # Merge each set of two sequential data sets together.
    data[[j]] <- rbind(dList[[(i * 2) - 1]], dList[[i * 2]])
    j <- j + 1
  }
  # In case length was odd, just add last set to the end.
  if (floor(len) != len) {
    data[[j]] <- dList[[len * 2]]
  }
  # Less data to store on the stack, tail call optimization would be nice here.
  # Try removing this and check out the time diff.
  rm(dList, len, j)
  # Recursive call.
  if (length(data) > 1) {
    data <- recurBind(data)
  }
  return(data)
}


###############################################################################
#
#       timer
#
###############################################################################
#' Time functions 
#'
#' Get the average time for a function call.
#'
#' @author Kenny Darrell
#' @return data.frame
#' 
#' @param df data frame under consideration
#' 
#' @keywords info
#' @export
#' @examples
#' data(orangeCar.test)
#' compRow(orangeCar.test)
#'

timer <- function(fun, iters, ...) {
  avgTime <- 0
  for (n in seq(iters)) {
    avgTime <- avgTime + system.time(fun(...))[3]
  }
  return(avgTime / iters)
}

# Less verbose but captures overhead of for loop.
timer2 <- function(fun, iters, ...) {
  system.time(for(n in seq(iters)) fun(...))[3] / iters
}


#####################################################################

#' Percent of complete rows 
#'
#' This displays the percentage of \code{\link{complete.cases}}
#'
#' @author Kenny Darrell
#' @return data.frame
#' 
#' @param df data frame under consideration
#' 
#' @keywords info
#' @export
#' @examples
#' data(orangeCar.test)
#' compRow(orangeCar.test)
#'
###
# Find missing values for each column
#
##
# TODO: whichMiss: document this function.
whichMiss <- function(data) 
{
  index   <- NULL
  count   <- NULL
  percent <- NULL
  name    <- NULL
  y <- nrow(data)
  for (i in 1:ncol(data))
  {
    x <- sum(is.na(data[,i]))
    if (x > 0) 
    {
      index   <- c(index,   i)
      count   <- c(count,   x)
      percent <- c(percent, x/y)
      name    <- c(name,    names(data)[i])
    }
  }
  miss <- list(index = index, count = count, percent = percent, name = name)
  return(miss)
}



#' Percent of complete rows 
#'
#' This displays the percentage of \code{\link{complete.cases}}
#'
#' @author Kenny Darrell
#' @return data.frame
#' 
#' @param df data frame under consideration
#' 
#' @keywords info
#' @export
#' @examples
#' data(orangeCar.test)
#' compRow(orangeCar.test)
#'

compCol <- function(df) { 
  col1 <- NULL
  col2 <- NULL
  for (i in 1:length(names(df))) {
    col1 <- rbind(col1, names(df)[i])
    col2 <- rbind(col2, sum(is.na(df[, i])))
  }
  col2 <- nrow(df) - col2
  data.frame(id = col1, nobs = col2)
}


#' Percent of complete rows 
#'
#' This displays the percentage of \code{\link{complete.cases}}
#'
#' @author Kenny Darrell
#' @return data.frame
#' 
#' @param df data frame under consideration
#' 
#' @keywords info
#' @export
#' @examples
#' data(orangeCar.test)
#' compRow(orangeCar.test)
#' 

compRow <- function(df) { 
  x <- sum(complete.cases(df))
  y <- x / nrow(df) * 100
  print("Percent of compelte rows")
  print(y)
  return(x)
}

safe.read.csv <- function(file, want, colTypes, ...) {
  # Error message to display.
  e <- simpleError("Import failed, file is missing needed field(s)!")
  # Get just the name of the file from the full loaction.
  name <- tail(unlist(strsplit(file, "/")), n = 1)
  # Message reading of file.
  cat("Reading ", name, "\n")
  # All of the fields in the file.
  have <- names(read.csv(file, nrow = 1))
  # Initialize the types which will be given to colClasses.
  type <- rep("NULL", length(have))
  # Do we have everything that we want in the file, exit otherwise.
  if (!all(want %in% have)) {
    # What is missing.
    missing <- setdiff(want, have)
    # DIsplay these fields and exit.
    tryCatch(stop(e), finally = print(missing))
  }
  # Initialize indices of fields we want.
  idx <- c()
  # Find if and where each field is in the file.
  for (field in want) {
    # Update indices with location of current desired field.
    idx <- c(idx, which(have %in% field))
  }
  # Replace NULL with type in the correct location.
  type[idx] <- colTypes
  # Read in data with only columns wanted.
  df <- read.csv(file, colClasses = type, ...)
  # Message to indicate successful read.
  cat("Succesfully read ", name, "\n")
  # Return the data that was read in.
  return(df)
}

