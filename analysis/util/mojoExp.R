###############################################################################
#
# Experimental stuff, data exploration  
#
###############################################################################

# Pull in library
library(chRonos)
# Pull data into workspace.
data(mvData)
# Get rid of really old data.
mov <- mvData[year(mvData$date) > 2002, ]
# Movies that have a first day.
wholeMov <- unique(mov[mov$Day == 1, ]$name)
# Create a list of movies in this set.
mov <- mov[mov$name %in% wholeMov, ]

library(ggplot2)
# Lets look at one movie.
title <- "X-Men: First Class"
movie <- mov[mov$name == title, ]
qplot(Day, Daily, data = movie, main = title)

# The day of the week seems to have a big impact on sales.
# Lets break it out into a plot for each day.
ggplot(movie, aes(Day, Daily)) + geom_point() + facet_grid(. ~ weekday)

# They seem to have a trend based on day of the week.
# We shoud make this more interactive so we can look at any movie.

library(gridExtra)
library(manipulate)
movPlot <- function(val) {
  weekDays <- ggplot(mov[mov$name == wholeMov[val], ], aes(Day, Daily)) +
    geom_point() + facet_grid(. ~ weekday)

  xx <- mov[mov$name == wholeMov[val], ]
  grouped <- qplot(Day, Daily, data = xx, main = wholeMov[val])
  grid.arrange(weekDays, grouped, ncol = 1)
}


manipulate(movPlot(Movie), Movie = slider(1, 600))

###############################################################################
#
#  Try to tokenize a movie
#
###############################################################################

# Use the movie from above.
m.Date <- mvData$date

#Turn movie into a time series.
x <- zoo(movie$daily, movie$date)
y <- zoo(movie$daily)
z <- zoo(log(movie$daily))
plot(x)

x1 <- sax(x, 5, 16, plot = TRUE)
y1 <- sax(y, 5, 16, plot = TRUE)
# Clearly a bug in lower right portion of this plot.
z1 <- sax(z, 15, 35, plot = TRUE)

movPlot <- function(val) {
  movie <- mov[mov$name == wholeMov[val], ]
  y <- zoo(movie$daily)
  sax(y, 15, 35, plot = TRUE)
}
manipulate(movPlot(Movie), Movie = slider(1, 600))


# Calander Heat Map

cal.heatMap(zoo(movie$daily, movie$date))
cal.heatMap(zoo(log(movie$daily), movie$date))
cal.heatMap(zoo(log(movie$daily), movie$date)[1:25])

###############################################################################
#
#  Stream chart
#
###############################################################################

ids <- chunk(mvData$date, 'month')
mvData1 <- merge(mvData, ids, by = 'date')
rm(ids)

#mvData1 <- mvData1[, c('date', 'name', 'daily', 'idx')]
# Test: The total number of movies up to time point x
ttply(mvData1, function(x) length(unique(x$name)), lag = 5)
plot(cc)

library(sqldf)

xx <- sqldf("
  select name, idx, sum(daily) as total
  from mvData1
  group by name, idx
")


yy <- xx[xx$idx %in% c(200:204), ]
d1 <- unique(yy$name)
d1 <- names(unlist(rev(sort(table(yy$name)))[1:20]))
da <- d1[1:10]
db <- d1[11:20]

p <- ggplot(yy, aes(x = idx))
p <- p + geom_area(subset = .(name %in% da),
                   aes(y = total, fill = name),
                   position = 'stack')
p <- p + geom_area(subset = .(name %in% db),
                   aes(y = -1 * total, fill = name),
                   position = 'stack')
p <- p + geom_hline(yintercept = 0)
p



###############################################################################
#
#  
#
###############################################################################



xx <- aggregate(mov[, c("daily")], by = list(mov$date), FUN = sum)
a.Date <- zoo(xx[, 2], xx[, 1])
cal.heatMap(a.Date)


transformers <- mov[mov$name == wholeMov[2299], ]


Quandl.push(code = 'TT0418279X', name = "Transformers", 
            desc = "Transformers Movie History", 
            data = transformers[, c('date', 'daily')], 
            override = TRUE,
            authcode = Quandl.auth("7vHUkUYRDQ2Tq4c1zZ62"))

###############################################################################
#
# comp
#
###############################################################################
names(mvData) <- c("date", "td", "yd", "name", "studio", "Daily", "peru",
                   "perd", "Theaters", "Avg", "Gross", "Day", "weekday")

names(nbData) <- c("date", "td", "yd", "name", "Distributor",
                   "Genre", "Daily", "Change", "Theaters", "PerThtr",
                   "Gross", "Day", "weekday")


av1 <- mvData[mvData$name == 'Avatar', ]
av2 <- nbData[nbData$name == 'Avatar', ]

#av1 <- av1[, c('date', 'Day', 'daily')]
#av2 <- av2[, c('date', 'Day', 'Gross')]

xx <- merge(av1, av2, by = c('date', 'name', 'weekday', 'Day', 'Daily', 'Theaters', 'td', 'yd', 'Gross'))


names(mvData)
names(nbData)
"Thtrs" to "Theaters"

###############################################################################
#
# IMDB JSON 
#
###############################################################################


# Get data from IMDb.

curMov <- movData[movData$date == (today()-5),3]

api <- "http://imdbapi.org/?q="
imdb <- NULL

for (mov in curMov) {
  mName <- gsub(" ", "+", mov, fixed = TRUE)
  
  site <- paste(api, mName, sep = "")
  
  mvJSON <- fromJSON(as.character(GET(site)))
  
  imdbJSON <- c(imdb, mvJSON)
}

rm(site, mvJSON, mName)

save(movies, movData, imdb, file = "mojo_imbd.RData")





###############################################################################
#
#  
#
###############################################################################




# make a better data structure that has all movie data in it
# Try to create and s4 class that encapsulates 
# a regular s3 time series class

scale <- movData[, c(3, 5, 10, 11, 12, 13)]
x <- scale[scale[,1 ] == "Avatar",]

x1 <- x[x[, 6] == "Sun",   c(2, 3, 4)]
x2 <- x[x[, 6] == "Mon",   c(2, 3, 4)]
x3 <- x[x[, 6] == "Tues",  c(2, 3, 4)]
x4 <- x[x[, 6] == "Wed",   c(2, 3, 4)]
x5 <- x[x[, 6] == "Thurs", c(2, 3, 4)]
x6 <- x[x[, 6] == "Fri",   c(2, 3, 4)]
x7 <- x[x[, 6] == "Sat",   c(2, 3, 4)]


#create flat table

mv <- movData
names(mv)[10] <- "Gross_To_Date"

flatMov <- sqldf("select name, studio, 
                 max(Gross_To_Date),
                 max(Day)
                 from mv
                 group by name")
names(flatMov)[3] <- "totalGross"
names(flatMov)[4] <- "length"
flatMov2 <- flatMov[flatMov[, 4] > 35, ]


###############################################################################
#
#  
#
###############################################################################


mongo <- mongoDbConnect("t", "localhost", 27017)

output <- dbInsertDocument(mongo, "test_data", '{"foo": "bar"}')

dbGetQuery(mongo, "test_data", 
           '{"name": "mongo"}')

output <- dbGetQueryForKeys(mongo, "test_data", 
                            '{"foo": "bar"}')



###############################################################################
#####
#
# Other movie sites
#http://www.the-numbers.com/box-office-chart/daily/2012/12/29
#http://www.boxofficeguru.com/g.htm
#http://boxofficequant.com/
#http://www.rottentomatoes.com/movie/box_office.php

#
#####
###############################################################################
load("mojo.RData")
library(lubridate)


movViz <- movData[, c(3, 5, 11, 12, 13)]
movViz$month <- month(movViz[,4])

movViz <- movViz[with(movViz, order(date)), ]

days <- unique(movViz[,4])
wdays <- unique(movViz[,4:5])

daily <- rep(NA, length(days))
j <- 1
for (i in days) {
  daily[j] <- sum(movViz[movViz[,4] == i, 2])
  j <- j + 1
  print(j)
}

sun <- wdays[,2] == "Sun"
mon <- wdays[,2] == "Mon"
tue <- wdays[,2] == "Tues"
wed <- wdays[,2] == "Wed"
thu <- wdays[,2] == "Thurs"
fri <- wdays[,2] == "Fri"
sat <- wdays[,2] == "Sat"



# This is chaotic, weekly may work better
# until I understand more about it.

wks <- ceiling(length(days) / 7) + 4
weekly <- rep(NA, wks)

j <- 1
k <- 1
wksum <- 0
for (i in days) {
  daily <- sum(movViz[movViz[,4] == i, 2])
  wksum <- wksum + daily 
  print(j)
  if (wdays[j, 2] == "Sat") {
    weekly[k] <- wksum
    wksum <- 0
    k <- k + 1
  }
  j <- j + 1
}

weekly <- weekly[1:sum(!is.na(weekly))]
# imdb refresh
# need to think through this, update only new movies,
# should this overwrite, does it even change



###############################################################################
#
#  
#
###############################################################################



library(rmongodb)

mongo <- mongo.create()

if (mongo.is.connected(mongo)) {
  print(mongo.get.databases(mongo))
  
}

for (i in mongo.get.databases(mongo)) {
  
  print(mongo.get.database.collections(mongo, i))
}

ns <- "test.people"
mongo.insert(mongo, ns, 5)




buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "age", 18L)
query <- mongo.bson.from.buffer(buf)

# Find the first 100 records
#    in collection people of database test where age == 18
cursor <- mongo.find(mongo, "test.people", query, limit=100L)
# Step though the matching records and display them
while (mongo.cursor.next(cursor))
  print(mongo.cursor.value(cursor))
mongo.cursor.destroy(cursor)


# shorthand: find all records where age=32, sorted by name, 
# and only return the name & address fields:
cursor <- mongo.find(mongo, "test.people", list(age=32),
                     list(name=1L), list(name=1L, address=1L))


###############################################################################
#
#  
#
###############################################################################


library(RMongo)

# Location of mongodb instance.
mongoIns <- "C:/mongodb/bin/mongod.exe"
# Start mongodb.
# this does not really work as it uses the shell
# and does not return.

#system(mongoIns)

mongo <- mongoDbConnect("t", "localhost", 27017)




output <- dbInsertDocument(mongo, "test_data", '{"foo": "bar"}')


output <- dbGetQuery(mongo, "test_data", 
                     '{"foo": "bar"}')

output <- dbGetQueryForKeys(mongo, "test_data", 
                            '{"foo": "bar"}', '{"foo": 1}')

