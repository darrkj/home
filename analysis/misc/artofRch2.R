#2.1 Scalers, Vectors, Arrays, and Matrices
x <- c(88, 5, 12, 13)
x <- c(x[1:3], 168, x[4])
length(x)

first1 <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] == 1) break
  }
  return(i)
}

#2.2 Declerations
z <- 3
y[1] <- 5   # does not work
y <- vector(length=2)  #creates logical vector of length two, false false
y[1] <- 5    #upgrades to numeric 5 0
y[2] <- 12

#Recycling
c(1, 2, 4) + c(6, 0, 9, 20, 22)
c(1, 2, 4, 1, 2) + c(6, 0, 9, 20, 22)  #the same as above

#2.4 Common Vector Operations
2+3
'+'(2, 3)
"+"(2, 3)
x <- c(1, 2, 4)
x + c(5, 0, -1)
x * c(5, 0, -1)  #not what you would expect from other matrix languages

x / c(5, 4, -1)
x %% c(5, 4, -1)
y <- c(1.2, 3.9, 0.4, 0.12)
y[c(1,3)]
y[2:3]
v <- 3:4
y[v]

x <- c(4,2,17,5)
y <- x[c(1,1,3)]
y

z <- c(5,12,13)
z[-1]
z[-1:-2]

z[-length(z)]   #remove last item of z

5:8
5:1

seq(from=1, by=2, to=10)
seq(1, 10, 2)
seq(0)
seq()
seq(NULL)


x<- rep(8, 4) # 8 8 8 8
rep(c(5, 12, 13), 3)    #5 12 13 5 12 13 5 12 13
rep(1:3, 2)

rep(c(5, 12, 13), each=3)

#Using all(0) and any()





































