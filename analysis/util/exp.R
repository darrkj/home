# Using SAX on all movie data

#library(boRg)

data(mvData)

x1 <- "What Happens in Vegas"
x2 <- mvData[mvData$name == x1, ]
x3 <- zoo(log(x2$Daily), x2$date)
x3a <- zoo(log(x2$Gross), x2$date)
n <- ceiling(length(x3) / 7) + 1
x4 <- sax(x3, 10, n, T)
x4a <- sax(x3a, 10, n, T)
x4 <- sax(x3, p=7, n, T)
x4a <- sax(x3a, p=7, n, T)

y1 <- "You, Me and Dupree"
y2 <- mvData[mvData$name == y1, ]
y3 <- zoo(log(y2$Daily), y2$date)
y3a <- zoo(log(y2$Gross), y2$date)
n <- ceiling(length(y3) / 7) + 1
y4 <- sax(y3, 10, n, T)
y4a <- sax(y3a, 10, n, T)
y4 <- sax(y3, p=7, n, T)
y4a <- sax(y3a, p=7, n, T)


a1 <- x3a[1:60]
a2 <- y3a[1:60]


library(dtw)

dtw(a1, a2)

## Find the best match
align <- dtw(a1, a2)

## Display the mapping, AKA warping function - may be multiple-valued
## Equivalent to: plot(alignment,type="alignment")
plot(align$index1, align$index2, main = "Warping function");

## Confirm: 25 samples off-diagonal alignment
lines(1:100, col = "red")

#####################################

names <- unique(mvData$name)

saxtest <- function(j = length(names)) {
  seqs <- rep(NA, j) 
  k <- 1
  data <- mvData[, c('name', 'Daily', 'date')]
  for (i in names[1:j]) {
    y <- data[data$name == i, ]
    y2 <- zoo(y$Daily, y$date)
    y3 <- sax(y2, bp = 10, p = 1, T)
    seqs[k] <- gsub(", ", "", toString(y3))
    k <- k + 1
  }
  return(seqs)
}

# Names are to crazy, would be better to have tt########
#names(seqs) <- names[1:j]

# names <- unique(mvData$name)
# 
# saxtest <- function(j = length(names)) {
#   seqs <- rep(NA, j) 
#   k <- 1
#   data <- mvData[, c('name', 'Daily', 'date')]
#   x <- lapply(names[1:j], function(x) data[data$name == x, ])
#   for (i in x) {
#     y <- zoo(i$Daily, i$date)
#     y2 <- sax(y, bp = 10, p = 7, T)
#     seqs[k] <- gsub(", ", "", toString(y2))
#     k <- k + 1
#   }
#   return(seqs)
# }


dd <- saxtest(10)

# Seems a little slow, profile
#library(rbenchmark)

Rprof("sax.out")
d <- saxtest(1000)
Rprof(NULL)
summaryRprof('sax.out')


d <- saxtest()

########################################
dist(x4, y4)
dist(y4, x4)
distTS(y3, x3)

dist(x4a, y4a)
distTS(y3a, x3a)

mov <- function(x) {
  x1 <- unique(mvData$name)[x]
  x2 <- mvData[mvData$name == x1, ]
  x3 <- zoo(log(x2$Daily), x2$date)
  x3a <- zoo(x2$Daily, x2$date)
  #n <- ceiling(length(x3) / 7) + 1
  #x4 <- sax(x3, 10, n, T)
  plot(x3a)
}


d <- d + 1
mov(d)



# Keep first 2000 movies by total number of days
names <- unlist(dimnames(rev(sort(table(mvData$name)))[1:2000]))
movies <- mvData[mvData$name %in% names, ]



mov <- function(x) {
  x1 <- unique(movies$name)[x]
  x2 <- movies[movies$name == x1, ]
  x3 <- zoo(log(x2$Daily), x2$date)
  x3a <- zoo(x2$Daily, x2$date)
  #n <- ceiling(length(x3) / 7) + 1
  #x4 <- sax(x3, 10, n, T)
  plot(x3a)
}

d <- 1

d <- d + 1
mov(d)


# get only first 30 days
movies2 <- movies[movies$Day < 31, ]

`%ni%` <- Negate(`%in%`)

mov <- function(x) {
  x1 <- unique(movies2$name)[x]
  x2 <- movies2[movies2$name == x1, ]
  x2 <- x2[x2$weekday %ni% c('Tues', 'Wed', 'Thurs'), ]
  x3 <- zoo(log(x2$Daily), x2$Day)
  x3a <- zoo(x2$Daily, 1:nrow(x2))
  #n <- ceiling(length(x3) / 7) + 1
  #x4 <- sax(x3, 10, n, T)
  plot(x3a)
}


dd <- ts(x2$Daily, frequency = 7)
m <- decompose(dd)
m$figure
plot(m)

d <- 1

d <- d + 1
mov(d)




require(graphics)

m <- decompose(co2)
m$figure
plot(m)

## example taken from Kendall/Stuart
x <- c(-50, 175, 149, 214, 247, 237, 225, 329, 729, 809,
       530, 489, 540, 457, 195, 176, 337, 239, 128, 102, 232, 429, 3,
       98, 43, -141, -77, -13, 125, 361, -45, 184)
x <- ts(x, start = c(1951, 1), end = c(1958, 4), frequency = 4)
m <- decompose(x)
## seasonal figure: 6.25, 8.62, -8.84, -6.03
round(decompose(x)$figure / 10, 2)


