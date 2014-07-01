r <- 1950:2050

holiday <- function(dm, t) {
  x <- as.Date(paste(dm, r, sep = ''), "%d%b%Y")
  y <- weekdays(x)
  x <- x + ifelse(weekdays(x) == 'Sunday', 1, ifelse(y == 'Saturday', -1, 0))   
  x <- data.frame(date = x, day = t)
}

hol <- function(y, m, n, d = 'Monday') {
  x <- as.Date(paste(1, m, y, sep = ''), "%d%b%Y") + 0:30
  as.character(x[weekdays(x) == d][n])
}


# New Year's Day
new <- holiday('1jan', 'New Years Day')

# Independence Day
ind <- holiday('4jul', 'Independence Day')

# Veterans Day
vet <- holiday('11oct', 'Veterans Day')

# Christmas Day
christ <- holiday('25dec', 'Christmas')

# Birthday of Martin Luther King, Jr.,  Third monday of january 
king <- data.frame(date = as.Date(unlist(lapply(r, function(y) hol(y, 'jan', 3)))), 
                   day = 'Birthday of Martin Luther King, Jr.')

# Washington's Birthday, Third monday of feb 
pres <- data.frame(date = as.Date(unlist(lapply(r, function(y) hol(y, 'feb', 3)))), 
                   day = "Washington's Birthday")

# Labor Day, first monday of sept
lab <- data.frame(date = as.Date(unlist(lapply(r, function(y) hol(y, 'sep', 1)))), 
                  day = "Labor Day")

# Columbus Day, first monday of sept
col <- data.frame(date = as.Date(unlist(lapply(r, function(y) hol(y, 'oct', 2)))), 
                  day = "Columbus Day")

# Thanksgiving Day, fourth thursday of nov
thank <- data.frame(date = as.Date(unlist(lapply(r, function(y) hol(y, 'nov', 4, d = 'Thursday')))), 
                    day = "Thanksgiving Day")


# Memorial Day, last monday of may
m <- function(y) {
  x <- as.Date(paste('1may', y, sep = ''), "%d%b%Y") + 0:30
  x <- as.character(x[weekdays(x) == 'Monday'])
  x <- x[length(x)]
}

mem <- data.frame(date = as.Date(unlist(lapply(r, m))), day = "Memorial Day")

h <- rbind(new, ind, vet, christ, king, pres, lab, col, thank, mem)
rm(new, ind, vet, christ, king, pres, lab, col, thank, mem, hol, r, m, holiday)

h <- h[order(h$date), ]
row.names(h) <- NULL
