library(Quandl)
library(zoo)
library(lubridate)
library(ggplot2)
library(plyr)
library(xts)

Quandl.auth("7vHUkUYRDQ2Tq4c1zZ62")

mydata = Quandl("NSE/OIL")

t1 = Quandl("NSE/OIL", type = "raw")
t2 = Quandl("NSE/OIL", type = "ts")
t3 = Quandl("NSE/OIL", type = "zoo")
t4 = Quandl("NSE/OIL", type = "xts")

t3 = Quandl("GOOG/NASDAQ_GOOG", type = "zoo")
GOOG/NASDAQ_GOOG
str(t3)