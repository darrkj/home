system.time(
  for (i in seq(500000)) {
    isPrime2(i)
  }
)

system.time(
  for (i in seq(500000)) {
    isPrime(i)
  }
)