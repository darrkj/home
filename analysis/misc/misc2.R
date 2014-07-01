# a vector iterator
i1 <- iter(1:3)
nextElem(i1)
nextElem(i1)
nextElem(i1)

# a vector iterator with a checkFunc
i1 <- iter(1:3, checkFunc=function(i) i %% 2 == 0)
nextElem(i1)

# a data frame iterator by column
i2 <- iter(data.frame(x=1:3, y=10, z=c('a', 'b', 'c')))
nextElem(i2)
nextElem(i2)
nextElem(i2)

# a data frame iterator by row
i3 <- iter(data.frame(x=1:3, y=10), by='row')
nextElem(i3)
nextElem(i3)
nextElem(i3)

# a function iterator
i4 <- iter(function() rnorm(1))
nextElem(i4)
nextElem(i4)
nextElem(i4)



i1 <- iter(1:20)
while(i1) {
  x <- seq(nextElem(i1), length.out=10)
  print(x)
  try(nextElem(i1))
}

# divide the value 10 into 3 pieces
it <- idiv(10, chunks=3)
nextElem(it)
nextElem(it)
nextElem(it)
try(nextElem(it))  # expect a StopIteration exception

# divide the value 10 into pieces no larger than 3
it <- idiv(10, chunkSize=3)
nextElem(it)
nextElem(it)
nextElem(it)
nextElem(it)
try(nextElem(it))  # expect a StopIteration exception


# Use the foreach package for the above.


# Create a FibonacciData object using attributes
x <- vector(mode="integer")
attr(x, "class") <- "FibonacciData"
x

# using the structure function

x <- structure(vector(mode="integer"), class="FibonacciData")
x

# using the class function

x <- vector(mode="integer")
class(x) <- "FibonacciData"
class(x)
# [1] "FibonacciData"


