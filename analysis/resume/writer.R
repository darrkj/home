`%eq%` <- function(a, b) {
  x <- as.character(substitute(a <- b))
  eval(substitute(a <<- b))
  eval(substitute(attr(a, 'trace') <<- paste(x[2], x[1], x[3])))
}

get_trace <- 

z %eq% 1

attributes(z)



foo <- function() {
  x <- 1
  y <- 2
  x + y
}

foo()

comment(x) <- c("This is my very important data from experiment #0234",
                "Jun 5, 1998")


# Could <- be called via setMethods (?) then call something like trace(x)
# which would then make x a class tracable then whenever you do something like
# x <- something(x) it runs the eq method.