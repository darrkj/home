# Data verification has a few things to consider

# Is the file in the correct place with the correct name, if not is the tool
# smart enough to see it above/below/close proximity to the file. Is is smart
# enough to wait until it gets the file to refresh.

# Format data comes in, Tucker may have built a function to do this,
# read a file and see if it has the same delimeter, or be able to modify the source
# code that is going to read it, change the sep option.

# Order of fields in the data, safe.read.csv already does this part.

# Name of fields, this is interesting, if the file has the same structure can we 
# rename the fields, Int(id), date, char, char, this could be the same in both
# sets but have different names.

# Names of the actual

# Content of the data.

# Has the data shifting far enough outside of the distributions that the original 
# assumptionas are no longer even valid. Was the model built in manner to adjust 
# or tune itself to account for this.

# Verification efforts should involve writing to some sort of config file, are the
# dates what was expected, should it align with last months data or is it a fresh
# pull every time.

# What levels of exit should the model have, skip over cases where its bad, don't
# score everything, should it just make things missing, should it exit altogether.



library(boRg)

data(orangeCar.train)
data(orangeCar.test)

x <- orangeCar.test
y <- orangeCar.train


names(x)
names(y)

v1 <- x[, 4]
v2 <- y[, 5]


getStats <- function(var) {
  type <- mode(var)
  if (type == 'numeric') {
    list(min = min(var),
         max = max(var),
         mean = mean(var),
         sd = sd(var))
  }
}

perDiff <- function(a, b) (a - b) / a

inRange <- function(a, b, per = .05) {
  x <- getStats(a)
  y <- getStats(b)
  apply(perDiff(x, y) < .05
  
}
#

















