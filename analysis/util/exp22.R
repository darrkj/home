# This file will be used to explore the data

source("util.R")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

xx <- NULL
for (i in 1:length(train))
{
  if (class(train[,i]) == "numeric" | class(train[,i]) == "integer")
  {
    xx[i] <- cor(y, train[,i])
  }
}

idx <- c(1:13, seq(14:))