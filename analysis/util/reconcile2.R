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

Love & Basketball tt0199725
The Edge tt0119051
Now & Later tt0865560
Blade tt0120611
Promised Land (2012) tt2091473
Promised Land tt0095916
Promised Lands tt0072041
Julie & Julia tt1135503
Julia tt0903627
Dance With Me tt0120576
Wolves tt1403241  tt0226648  tt2311460
Bodyguard tt1729637
Interview tt0360674 tt0480269
Vampires tt1500906  tt0120877
-------------
The Assignment tt0118647
The Terrorist tt0118647
Face tt0119094
Face/Off tt0119094
Grosse Pointe Blank tt0119229
Point Blank tt0119229
Angel-A tt0119488
L.A. Confidential tt0119488
Men in Black tt0119654
In & Out tt0119654
My Best Friend tt0119738
4103    My Best Friend's Wedding tt0119738

Toy Story 2 tt0120363
3069Toy Story / Toy Story 2 (3D) tt0120363
4821 Toy Story 3D Double Feature tt0120363
Hit & Run tt0120609
4529   The Big Hit tt0120609
3597Enemy of the State tt0120660
4729 The Statement tt0120660


Phantoms tt0120915
Patch Adams tt0129290
3219  Adam & Steve tt0129290
The Talented Mr. Ripley tt0134119
4593  The Gift tt0134119

Rage tt0144814
4708 The Rage: Carrie 2 tt0144814
3523 Deep Blue tt0149261
3524 Deep Blue Sea tt0149261

Charlie's Angels tt0160127
3438 Charly tt0160127

Along Came a Spider tt0164334
2846 Spider tt0164334


The King tt0167260
The Cell tt0167261

American Beauty tt0169547
3324 Beautiful tt0169547
178    Bad Boys II tt0172156
2334  Bad Boys tt0172156
2370 Boy A tt0172156
2661   Light it Up tt0172726
3967   Light It Up tt0172726

November tt0230838
4472    Sweet November tt0230838

Spirited Away tt0245429
4360 Senna tt024542

Bubble tt0258470
3402    Bubble Boy tt0258470
646    Ghost Rider tt0259324
4298Ride tt0259324


The Life of David Gale tt0289992
3509 David & Layla tt0289992


Harry Potter and the Chamber of Secrets tt0295297
3203  A Secret tt0295297



























# 
# x <- unique(nbData$name)
# y <- unique(mvData$name)
# z <- intersect(x, y)
# rm(x, y)
# 
# # add other movies
# m <- setdiff(nbData$name, imdb$name)
# mvimdb <- data.frame(name = m, key = rep(NA, length(m)))
# 
# imdb <- rbind(imdb, mvimdb)
# 
# #imdb <- data.frame(name = z, key = rep(NA, length(z)))
# x <- mov
# imdb$key <- ifelse(imdb$key == -1, NA, imdb$key)
# needed <- imdb[is.na(imdb$key), ]$name
# 
# 
# http://mymovieapi.com/?id=tt1130884
# http://mymovieapi.com/?title=shutter+island
# http://mymovieapi.com/?title=transformers