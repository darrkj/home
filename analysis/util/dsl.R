library(boRg)
data(mvData)
title <- "Avatar"
movie <- mvData[mvData$name == title, ]
a.Date <- zoo(movie$Daily, movie$date)
cal.heatMap(a.Date)


title <- "Avatar"
movie1 <- mvData[mvData$name == title, ]$date
title <- "A Beautiful Mind"
movie2 <- mvData[mvData$name == title, ]$date


movie1 %before% movie2
movie2 %before% movie1

###############

"@" <- function(name, var) {
  mvData[mvData$name == name, var]
}

`@`('Avatar', 'date')
'Avatar' @ 'date' %before% 'A Beautiful Mind' @ 'date'
'A Beautiful Mind'@'date' %before% 'Avatar'@'date'

#################

`%of%` <- function(var, name) {
  mvData[mvData$name == name, var]
}

('date' %of% 'Avatar') %before% ('date' %of% 'A Beautiful Mind')
('date' %of% 'A Beautiful Mind') %before% ('date' %of% 'Avatar')

# If it will always be date, the second argument could be a data.frame
# But then in loses the written grammer style.

####################
# Find all true relations between two.
'Avatar' @ 'date' %before% 'A Beautiful Mind' @ 'date'

resolve <- function(x, y) {
  mnx <- min(x @ 'date')
  mxx <- max(x @ 'date')
  mny <- min(y @ 'date')
  mxy <- max(y @ 'date')
  return(list(mnx, mxx, mny, mxy))
}

resolve('Avatar', 'A Beautiful Mind')


'Avatar' @ 'date' %before% 'A Beautiful Mind' @ 'date'
'Avatar' @ 'date' %after% 'A Beautiful Mind' @ 'date'
'Avatar' @ 'date' %meets% 'A Beautiful Mind' @ 'date'
'Avatar' @ 'date' %overlaps% 'A Beautiful Mind' @ 'date'
'Avatar' @ 'date' %starts% 'A Beautiful Mind' @ 'date'
'Avatar' @ 'date' %during% 'A Beautiful Mind' @ 'date'
'Avatar' @ 'date' %finishes% 'A Beautiful Mind' @ 'date'
'Avatar' @ 'date' %equal% 'A Beautiful Mind' @ 'date'

'A Beautiful Mind' @ 'date' %before% 'Avatar' @ 'date'
'A Beautiful Mind' @ 'date' %after% 'Avatar' @ 'date'
'A Beautiful Mind' @ 'date' %meets% 'Avatar' @ 'date'
'A Beautiful Mind' @ 'date' %overlaps% 'Avatar' @ 'date'
'A Beautiful Mind' @ 'date' %starts% 'Avatar' @ 'date'
'A Beautiful Mind' @ 'date' %during% 'Avatar' @ 'date'
'A Beautiful Mind' @ 'date' %finishes% 'Avatar' @ 'date'
'A Beautiful Mind' @ 'date' %equal% 'Avatar' @ 'date'

################


"∑" <- function(x) sum(x)
`∑`(1:10)

