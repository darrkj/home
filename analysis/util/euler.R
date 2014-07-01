###################################################################
#
#          Project Euler Problem 8
#
#   A Pythagorean triplet is a set of three natural numbers, 
#      a  b  c, for which,
#
#         a^2 + b^2 = c^2
#         For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
#
#   There exists exactly one Pythagorean triplet for which 
#              a + b + c = 1000.
#         Find the product abc.
#
###################################################################

x <- seq(1, 31)
y <- x * x
a <- 1
b <- 1
c <- 1
for (i in 1:31) {
  for (j in 1:31) {
    for (k in 1:31){
      val <- i * j * k
      if (val == 1000) {
        abc <- a * b * c
        break
      }
    }
  }
}





###################################################################
#
#          Project Euler Problem 8
#
#   Find the greatest product of five consecutive digits 
#      in the 1000-digit number.
#
###################################################################


x <- '7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450'

ll <- c()
for (i in 1:995){
  x1 <- as.numeric(substr(x, i, i)) 
  x2 <- as.numeric(substr(x, i+1, i+1))
  x3 <- as.numeric(substr(x, i+2, i+2))
  x4 <- as.numeric(substr(x, i+3, i+3))
  x5 <- as.numeric(substr(x, i+4, i+4))
  ll <- c(ll, (x1*x2*x3*x4*x5))
}
yy <- max(ll)

###################################################################
#
#          Project Euler Problem 6
#
#   The sum of the squares of the first ten natural numbers is,
#            1^2 + 2^2 + ... + 10^2 = 385
# The square of the sum of the first ten natural numbers is,
#
# (1 + 2 + ... + 10)^2 = 55^2 = 3025
# Hence the difference between the sum of the squares of the first 
# ten natural numbers and the square of the sum is 3025-385 = 2640.
#
# Find the difference between the sum of the squares of 
# the first one hundred natural numbers and the square of the sum.
#
###################################################################

x <- sum(seq(1, 100))^2 - sum(seq(1, 100) * seq(1, 100))

x == 25164150

###################################################################
#
#          Project Euler Problem 5
#
#   2520 is the smallest number that can be divided by each
#     of the numbers from 1 to 10 without any remainder.
#     What is the smallest positive number that is evenly 
#     divisible by all of the numbers from 1 to 20?
#
###################################################################
i = 1
max <- 20
x <- seq(1, max)
j <- max
while (i == 1) {
  j <- j + max
  y <- sum('%%'(j, x))
  if (y == 0) {
    val = j
    i = 0
  }
}

val == 232792560
###################################################################
#
#          Project Euler Problem 5
#
#   2520 is the smallest number that can be divided by each
#     of the numbers from 1 to 10 without any remainder.
#     What is the smallest positive number that is evenly 
#     divisible by all of the numbers from 1 to 20?
#
###################################################################