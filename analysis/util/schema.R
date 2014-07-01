
###############################################################################
#
#     combs
#
###############################################################################
#' Take all possible combinations
#' 
#' This function takes a number (integer) and returns all of the combinations
#' of choosing 2 elements where order does not matter.  The returned object 
#' is a set indices to use on another object where you want to apply a function
#' to every combination.
#' 
#' @author Kenny Darrell
#' @return Dataframe with two rows of each possible unique combination
#' @param x The number of items in the set
#' @keywords utils
#' @export
#' @examples
#' combs(5)
#' 

combs <- function(x) {
  # Initialize the returned object to null.
  ret <- NULL
  # There is no step needed for x as all others below 
  # x will touch it in there step.
  for(i in seq(x - 1)) {
    # Add the results from expand grid to the growing index list.
    ret <- rbind(ret, expand.grid(i, (i + 1):x))
  }
  return(ret)
}


###############################################################################
#
#     Learn.Schema
#
###############################################################################
#' Take all possible combinations
#' 
#' This function takes a list of tables and will return a graph that 
#' shows how the tables are linked together.  This will show you how you
#' can add different fields from different tables on each other.
#' 
#' @author Kenny Darrell
#' @return A graph which shows the linkages of tables.
#' @param tableList A list of table which you want to reconcile
#' @param ignore A field to leave out of the schema
#' @keywords utils
#' @export
#' @examples
#' a <- data.frame(a = c(1, 2, 3) , b = c(2, 3, 4), c = c(1, 2, 3))
#' b <- data.frame(a = c(1, 2, 3) , d = c(2, 3, 4))
#' c <- data.frame(b = c(1, 2, 3) , e = c(2, 3, 4))
#' d <- data.frame(c = c(1, 2, 3) , f = c(2, 3, 4))
#' e <- data.frame(d = c(1, 2, 3) , g = c(2, 3, 4))
#' 
#' yyy <- list(a = a, b = b, c = c, d = d, e = e)
#' x <- Learn.Schema(yyy)
#' plot(x)
#' 
#' y <- Learn.Schema(yyy, ignore = 'd')
#' plot(y)
#' 

Learn.Schema <- function(tableList, ignore = c()) {
  name <- lapply(tableList, names)
  len <- length(name)
  cmb <- combs(len)
  schema <- graph.empty() + vertices(names(tableList))
  for(i in 1:nrow(cmb)) {
    a <- cmb[i, 1]
    b <- cmb[i, 2]
    if(length(setdiff(intersect(name[[a]], name[[b]]), ignore)) > 0) {
      schema <- schema + edge(names(tableList)[a], names(tableList)[b])
    }
  }
  return(schema)
}

###############################################################################
#
#     Get.Files
#
###############################################################################
#' Read all files in a directory
#' 
#' This function takes a directory and returns a list data.frames 
#' of all of the files in the directory
#' 
#' @author Kenny Darrell
#' @return A list of data.frames
#' @param direct A directory in which to read all files
#' @param delim The delimeter in your file
#' @keywords utils
#' @export
#' @examples
#' xxx <- Get.Files("tdata")
#' xxx <- Get.Files("tdata", ",")

Get.Files <- function(direct, delim = "|") {
  # This is all of the files.
  files <- list.files(direct)
  # Join the file name to the dir.
  newFiles <- paste(direct, files, sep = "/")
  # Initialize arrays for names of fields.
  tables <- NULL
  # Loop over these files and append to a growing list of names.
  for (i in seq(newFiles)) {
    tables[[i]] <- read.csv(newFiles[i], sep = delim, header = TRUE)#, nrows = 1)
  }
  names(tables) <- files
  return(tables)
}

###############################################################################
#
#     setLCD
#
###############################################################################
#' Find common item in list of sets
#' 
#' If you have a list of tables there could be fields like
#' 'ID', or 'Modified_Date', or 'Modified_By' which you would not
#' want to join (in most cases) as it is really more like field metadata
#' not relational data. This function will find fields of this type that are
#' common across all tables.
#' 
#' @author Kenny Darrell
#' @return A list of fields which exists in all tables
#' @param tableList A list of data.frames
#' @keywords utils
#' @export
#' @examples
#' a <- data.frame(a = c(1, 2, 3) , b = c(2, 3, 4), id = c(1, 2, 3))
#' b <- data.frame(a = c(1, 2, 3) , d = c(2, 3, 4), id = c(1, 2, 3))
#' c <- data.frame(b = c(1, 2, 3) , e = c(2, 3, 4), id = c(1, 2, 3))
#' 
#' yyy <- list(a = a, b = b, c = c)
#' setLCD(yyy)
#' 

setLCD <- function(tableList) {
  # Initialise contents
  lcd <- names(tableList[[1]])
  # Loop through all sets
  for (i in seq(length(tableList))) {
    # Intersect with the current LCD
    lcd <- intersect(lcd, names(tableList[[i]]))
  }
  return(lcd)
}



Drop.LCD <- function(tableList, drop = c()) {
  drop <- union(setLCD(tableList), drop)
  i <- 1
  for (i in seq(tableList)) {
    data <- tableList[[i]]
    keep <- setdiff(names(data), intersect(names(data), drop))
    tableList[[i]] <- data[, keep]
  }
  return(tableList)
}

Not.Connected <- function(schema) {
  notCon <- degree(schema)
  list(delete.vertices(schema, names(notCon[notCon == 0])),
       names(notCon[notCon == 0]))
}

drop.extremes <- function(schema) {
  Not.Connected(schema)[[1]]
}

is.Star <- function(schema) {
  deg <- degree(schema)
  if (max(deg) == (length(deg) - 1) & sum(deg == 1) == (length(deg) - 1)) 
    TRUE else FALSE
}


# flatten <- function(schema) {
#   
#   
# }

# 
# a <- data.frame(a = c(1, 2, 3) , b = c(2, 3, 4), c = c(1, 2, 3))
# b <- data.frame(a = c(1, 2, 3) , d = c(2, 3, 4))
# c <- data.frame(b = c(1, 2, 3) , e = c(2, 3, 4))
# d <- data.frame(c = c(1, 2, 3) , f = c(2, 3, 4))
# e <- data.frame(d = c(1, 2, 3) , g = c(2, 3, 4))
# 
# yyy <- list(a = a, b = b, c = c, d = d, e = e)
# x <- Learn.Schema(yyy)
# plot(x)
# 
# y <- Learn.Schema(yyy, ignore = 'd')
# plot(y)
# 
# setLCD(yyy[1:2])
# 
# 
# z <- drop.extremes(y)
# plot(z)
# 
# 
# is.Star(y)
# is.Star(x)
# is.Star(z)
# 
# 
