library(rjson)
library(httr)

options(stringsAsFactors = FALSE)

#load("~/Desktop/chronos/chRonos/data/nbData.RData")

# Convoluted process to get IMDb tt value from google.
getTT <- function(name) {
  # This the basic google search url.
  q <- "https://www.google.com/search?q=imdb+"
  # Add the name of the movie the end of the search.
  query <- paste(q, name, sep = "")
  # Get the resutls of the query in HTML and turn it into text. Split 
  # this up by spaces and turn it into a character array.
  x <- unlist(strsplit(as.character(GET(query)), split = "\ "))
  # look for the IMDb entry which should be towards teh top and take the first.
  z <- grep("www.imdb.com/title/tt", x, value = T)[1]
  # Split these by slashes, ex google.com/x/tt3242342/page
  z <- unlist(strsplit(as.character(z), split = "/"))
  z <- grep("^tt", z, value = T)
  # Take the tt number out.
  return(z)
}


# process to get IMDb tt value from imdb API.
getTT2 <- function(name) {
  # This the basic api search url.
  q <- "http://mymovieapi.com/?title="
  # Add the name of the movie to the api call.
  query <- paste(q, name, sep = "")
  # move responce to json, and get id.
  x <- fromJSON(as.character(GET(query)))[[1]]
  if(class(x) != 'list') {
    return()
  }
  return(x$imdb_id)
}
# This seemed to get those that google did not except those that contain
# and '&' in the name.

query <- function() {
  for (mov in imdb[is.na(imdb$key), ]$name) {
    print(mov)
    mov2 <- gsub('&', '%20&%20', mov)
    t <- getTT(mov2)
    if (length(t) != 0) {
      imdb[imdb$name == mov, ]$key <- t
      print(imdb[imdb$name == mov, ])
    }
    Sys.sleep(5)
  }
}



reps <- imdb[imdb$key %in% names(which(table(imdb$key) > 1)), ]
reps <- reps[with(reps, order(key)), ]
