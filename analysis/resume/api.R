# Collect all win/loss data for NBA history.
library(XML)
library(httr)
options(stringsAsFactors = FALSE)

x1 <- 'http://stackoverflow.com/users/2179509/darrelkj'

x <- readHTMLTable(x1)
x <- GET(x1)
x <- as.character(GET(x1))
x <- strsplit(as.character(GET(x1)), split = "\ ")
x <- unlist(strsplit(as.character(GET(x1)), split = "\n"))
xx <- x[x != '']

grep('visited', xx)


y1 <- 'http://stats.stackexchange.com/users/8772/darrelkj'