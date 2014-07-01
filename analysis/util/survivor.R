players <- c("Denise", "Michael", "Lisa", "Malcolm", "Abi-Maria", "Carter",
             "Jonathan", "Pete", "Artis", "Jeff", "RC", "Katie", "Dawson",
             "Dana", "Russell", "Angie", "Roxy", "Zane")


# Agent based simulation

vote <- function(players) {
  votes <- c()
  for (i in players) {
    votes <- c(votes, sample(setdiff(players, i), 1))
  }
  return(votes)
}

elimination <- function(votes, idol = c()) {
  
}