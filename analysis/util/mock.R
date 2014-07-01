###############################################################################
#
#     Zip.Codify
#
###############################################################################
#' Numeric zip to string
#' 
#' This function will take care of the problem of having a zip code which
#' comes in numeric form not having leading zeros.
#' 
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @return Zip code in string format with leading zeros
#' @param zip A zip code which is of numeric type
#' @keywords utils
#' @export
#' @examples
#' zip <- 234
#' Zip.Codify(zip)
#' # Zip.Codify

Zip.Codify <- function(zip) {
  # Turn Zipcode into character field, if not already. 
  zip <- as.character(zip)
  # Replace # with 0000# 
  zip <- ifelse(nchar(zip) == 1, paste("0000", zip, sep=""), zip)
  # Replace ## with 000##  
  zip <- ifelse(nchar(zip) == 2, paste("000", zip, sep=""), zip)
  # Replace ### with 00### 
  zip <- ifelse(nchar(zip) == 3, paste("00", zip, sep=""), zip)
  # Replace #### with 0#### 
  zip <- ifelse(nchar(zip) == 4, paste("0", zip, sep=""), zip)
  return(zip)
}

##############################################################################
#
#     mock.person
#
###############################################################################
#' Generate fake people data
#' 
#' This function will return a list of names generated at random.
#' 
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @return A vector of peoples names
#' @param len The desired number of people to generate
#' @keywords mock
#' @export
#' @examples
#' people <- mock.person(5)
#' 

mock.person <- function(len) {
  first.name <- c("Kenny", "Chris", "Matt", "Mike", "Jason", "Sarah", "Erika",
                  "Beth", "Marie", "Linda", "Alex", "Heather","Robert", "Mark",
                  "Dan", "Anne", "Stacey", "Emily", "Jessica", "Tom", "Steve",
                  "Katy", "Melissa", "Mary")
  last.name <- c("Smith", "Brown", "Thompson", "Alpert", "Lin", "Hawkins",
                 "Jones", "Peterson", "Brooks", "Jennings", "Strick",
                 "Kennedy", "Marshall")
  person <- NULL
  for (i in seq(len)) {
    newPerson <- paste(sample(last.name, 1), sample(first.name, 1), sep = ", ")
    person <- c(person, newPerson)
  }
  person <- data.frame(name = person)
  return(person)
}

mock.age <- function(len) {
  rpois(len, 30)
}

mock.zip <- function(len) {
  Zip.Codify(sample(5:94000, len, replace = TRUE))
}

mock.word <- function() {
  first <- toupper(sample(letters, 1))
  rest <- paste(sample(letters, rpois(1, 7)), collapse = "")
  paste(first, rest, sep = "")
}



mock.address <- function(len) {
  loc <- c("Street", "Lane", "st", "Way", "Park", "Court")
  x <- data.frame(address = rep(NA, len))
  for (j in seq(len)) {
    address <- sample(loc, 1, replace = TRUE)
    for (i in 1:sample(1:2, 1)) {
      address <- c(address, mock.word())    
    }
    address <- c(address, as.character(sample(1:9999, 1))) 
    x[j, ] <-paste(rev(address), collapse = " ")
  }
  return(x)
}



mock.state <- function(len) {
  data(state)
  sample(state.abb, len, replace = TRUE)
}


mock.date <- function(len) {
  as.Date(substr(Sys.time() - 
                   sample(1:9999, len, , replace = TRUE) * 80000, 1, 10))
}

mock.metric <- function(len) {
  runif(len, 0, 10)
}

mock.type <- function(len, set) {
  sample(set, len, replace = TRUE)
}

# Check that this is not horrible slow.
create.data <- function(n) {
  data.frame(id = seq(n), 
             name = mock.person(n), 
             age = mock.age(n),
             zip = mock.zip(n),
             address = mock.address(n),
             state = mock.state(n),
             date = mock.date(n))
}

create.tcss <- function(n) {
  data.frame(id = seq(n), 
             name = mock.person(n),
             type1 = mock.type(n, c("Highway", "Box", "Combo")),
             type2 = mock.type(n, c("Emergency", "Normal", "Temp")),
             t1 = mock.metric(n),
             t2 = mock.metric(n),
             t3 = mock.metric(n),
             f1 = mock.metric(n),
             f2 = mock.metric(n),
             f3 = mock.metric(n),
             date.cr = mock.date(n))
}


# system.time(create.data(10))
# system.time(create.data(100))
# system.time(create.data(1000))
# system.time(create.data(10000))
# system.time(create.data(20000))
# system.time(create.data(50000))
# 
# library(profr)
# 
# Rprof("mock.out")
# # Call the function to be profiled
# y = create.data(10000)
# Rprof(NULL)
# summaryRprof("mock.out")

# The above is good for now for a base set.
# Need to add stuff for things over time.


increment <- function(data, pois) {
  len <- nrow(data)
  newdata <- NULL
  for (i in seq(len)) {
    x <- rpois(1, pois)
    newdata <- rbind(newdata, cbind(data[i,], 1:x))
  }
}
