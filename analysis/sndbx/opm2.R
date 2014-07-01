holiday <- function(from, to) {
  r <- from:to
  hol1 <- function(dm, t) {
    x <- as.Date(paste(dm, r, sep = ''), "%d%b%Y")
    y <- weekdays(x)
    x <- x + ifelse(weekdays(x) == 'Sunday', 1, ifelse(y == 'Saturday', -1, 0))   
    data.frame(date = x, day = t)
  }
  
  x <- function(y, m, n, d = 'Monday') {
    x <- as.Date(paste(1, m, y, sep = ''), "%d%b%Y") + 0:30
    as.character(x[weekdays(x) == d][n])
  }
  
  hol2 <- function(m, n, t, ...) data.frame(
    date = as.Date(unlist(lapply(r, function(y) x(y, m, n, ...)))), day = t)
  
  mem <- function(y) {
    x <- as.Date(paste('1may', y, sep = ''), "%d%b%Y") + 0:30
    tail(as.character(x[weekdays(x) == 'Monday']), 1)
  }
  
  h <- rbind(hol1('1jan', 'New Years Day'), hol1('4jul', 'Independence Day'),
             hol1('11oct', 'Veterans Day'), hol1('25dec', 'Christmas'),
             hol2('jan', 3, 'Birthday of Martin Luther King, Jr.'),
             hol2('feb', 3, "Washington's Birthday"),
             hol2('sep', 1, "Labor Day"), hol2('oct', 2, "Columbus Day"),
             hol2('nov', 4, "Thanksgiving Day", d = 'Thursday'),
             data.frame(date = as.Date(unlist(lapply(r, mem))), day = "Memorial Day")            
  )
  
  h <- h[order(h$date), ]
  row.names(h) <- NULL
  h
}
